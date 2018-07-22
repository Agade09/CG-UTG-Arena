#include <iostream>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <poll.h>
#include <array>
#include <random>
#include <list>
#include <chrono>
#include <omp.h>
#include <limits>
#include <algorithm>
#include <map>
#include <queue>
#include <thread>
#include <csignal>
using namespace std;
using namespace std::chrono;

constexpr int N{2};//Number of players
constexpr bool Debug_AI{false},Timeout{false};
constexpr int PIPE_READ{0},PIPE_WRITE{1};
constexpr double FirstTurnTime{1*(Timeout?1:10)},TimeLimit{0.1*(Timeout?1:10)};

bool stop{false};//Global flag to stop all arena threads when SIGTERM is received

constexpr int Spread_Units{5};

struct planet{
    array<int,2> tolerance,units;
    vector<int> L;
    inline int owner()const{
        if(units[0]==units[1]){
            return -1;
        }
        return units[0]>units[1]?0:1;
    }
};

struct action{
    array<int,5> affects;//ID of the five planets to affect
    int spread;//ID of the planet to spread from
    inline bool operator!=(const action &a)const{
        for(int i=0;i<5;++i){
            if(affects[i]!=a.affects[i]){
                return true;
            }
        }
        return spread!=a.spread;
    }
    inline bool operator==(const action &a)const{
        for(int i=0;i<5;++i){
            if(affects[i]!=a.affects[i]){
                return false;
            }
        }
        return spread==a.spread;
    }
};

struct state{
    vector<planet> P;//All planets indexed by id
    inline void Possible_Affects(vector<int> &Possible,const int id)const{
        Possible.clear();
        for(int i=0;i<static_cast<int>(P.size());++i){
            if(P[i].owner()==id || P[i].units[id]>0){
                if(P[i].tolerance[id]>0 && find(Possible.begin(),Possible.end(),i)==Possible.end()){
                    Possible.push_back(i);
                }
            }
            if(P[i].units[id]>0){
                for(const int n_id:P[i].L){
                    if(P[n_id].tolerance[id]>0 && find(Possible.begin(),Possible.end(),n_id)==Possible.end()){
                        Possible.push_back(n_id);
                    }
                }
            }
        }
    }
    void simulate(const array<action,2> &moves){
        //Affectations
        array<vector<int>,2> Possible_Affectations;
        for(int i=0;i<2;++i){
            Possible_Affects(Possible_Affectations[i],i);
        }
        for(int i=0;i<2;++i){
            vector<int> affected;
            for(const int id:moves[i].affects){
                if(id<0 || id>=static_cast<int>(P.size())){
                    cerr << "AI " << i << " requested affect on planet id " << id << " but there are " << P.size() << " planets." << endl;
                    throw(3);
                }
                if(P[id].tolerance[i]>0 && find(Possible_Affectations[i].begin(),Possible_Affectations[i].end(),id)!=Possible_Affectations[i].end()){
                    ++P[id].units[i];
                    if(find(affected.begin(),affected.end(),id)==affected.end()){
                        affected.push_back(id);
                    }
                }
                else if(P[id].tolerance[i]==0){
                    //cerr << "Warning: AI " << i << " tried to affect to planet " << P[id].tolerance[i] << " where its tolerance is 0" << endl;
                }
                else if(find(Possible_Affectations[i].begin(),Possible_Affectations[i].end(),id)==Possible_Affectations[i].end()){
                    //cerr << "Warning: AI " << i << " tried to affect to a planet it cannot reach" << endl; 
                }
            }
            for(const int id:affected){
                P[id].tolerance[i]=max(0,P[id].tolerance[i]-1);
            }
        }
        //Spread
        for(int i=0;i<2;++i){
            if(moves[i].spread!=-1){
                if(P[moves[i].spread].units[i]<Spread_Units){
                    cerr << "AI " << i << " tried to spread from planet " << moves[i].spread << " with " << P[moves[i].spread].units[i] << " units." << endl;
                    throw(3);
                }
                P[moves[i].spread].units[i]-=Spread_Units;
                for(const int id:P[moves[i].spread].L){
                    P[id].units[i]+=1;
                }
            }
        }
        //Combat
        vector<int> My_Losses,Enemy_Losses;
        for(int i=0;i<static_cast<int>(P.size());++i){
            int allies_ctr{0};
            for(const int id:P[i].L){
                if(P[id].owner()==0){
                    ++allies_ctr;
                }
                else if(P[id].owner()==1){
                    --allies_ctr;
                }
            }
            if(allies_ctr<0){
                My_Losses.push_back(i);
            }
            else if(allies_ctr>0){
                Enemy_Losses.push_back(i);
            }
        }
        for(const int id:My_Losses){
            P[id].units[0]=max(0,P[id].units[0]-1);//Is the max actually necessary?
        }
        for(const int id:Enemy_Losses){
            P[id].units[1]=max(0,P[id].units[1]-1);//Is the max actually necessary?
        }
    }
    string to_string()const{
        stringstream ss;
        ss << P.size() << " ";
        for(int i=0;i<static_cast<int>(P.size());++i){
            const planet &p{P[i]};
            for(int j=0;j<2;++j){
                ss << p.tolerance[j] << " " << p.units[j] << " ";
            }
            ss << P[i].L.size() << " ";
            for(const int id:P[i].L){
                ss << id << " ";
            }
        }
        return ss.str();
    }
    void load_string(const string &s){
        stringstream ss(s);
        int planetCount;
        ss >> planetCount;
        P.resize(planetCount);
        for(int i=0;i<static_cast<int>(P.size());++i){
            planet &p{P[i]};
            for(int j=0;j<2;++j){
                ss >> p.tolerance[j] >> p.units[j];
            }
            int link_count;
            ss >> link_count;
            p.L.clear();
            for(int j=0;j<link_count;++j){
                int id;
                ss >> id;
                p.L.push_back(id);
            }
        }
    }
};

inline string EmptyPipe(const int fd){
    int nbytes;
    if(ioctl(fd,FIONREAD,&nbytes)<0){
        throw(4);
    }
    string out;
    out.resize(nbytes);
    if(read(fd,&out[0],nbytes)<0){
        throw(4);
    }
    return out;
}

struct AI{
    int id,pid,outPipe,errPipe,inPipe,turnOfDeath;
    string name;
    inline void stop(const int turn=-1){
        if(alive()){
            kill(pid,SIGTERM);
            int status;
            waitpid(pid,&status,0);//It is necessary to read the exit code for the process to stop
            if(!WIFEXITED(status)){//If not exited normally try to "kill -9" the process
                kill(pid,SIGKILL);
            }
            turnOfDeath=turn;
        }
    }
    inline bool alive()const{
        return kill(pid,0)!=-1;//Check if process is still running
    }
    inline void Feed_Inputs(const string &inputs){
        if(write(inPipe,&inputs[0],inputs.size())!=inputs.size()){
            throw(5);
        }
    }
    inline ~AI(){
        close(errPipe);
        close(outPipe);
        close(inPipe);
        stop();
    }
};

void StartProcess(AI &Bot){
    int StdinPipe[2];
    int StdoutPipe[2];
    int StderrPipe[2];
    if(pipe(StdinPipe)<0){
        perror("allocating pipe for child input redirect");
    }
    if(pipe(StdoutPipe)<0){
        close(StdinPipe[PIPE_READ]);
        close(StdinPipe[PIPE_WRITE]);
        perror("allocating pipe for child output redirect");
    }
    if(pipe(StderrPipe)<0){
        close(StderrPipe[PIPE_READ]);
        close(StderrPipe[PIPE_WRITE]);
        perror("allocating pipe for child stderr redirect failed");
    }
    int nchild{fork()};
    if(nchild==0){//Child process
        if(dup2(StdinPipe[PIPE_READ],STDIN_FILENO)==-1){// redirect stdin
            perror("redirecting stdin");
            return;
        }
        if(dup2(StdoutPipe[PIPE_WRITE],STDOUT_FILENO)==-1){// redirect stdout
            perror("redirecting stdout");
            return;
        }
        if(dup2(StderrPipe[PIPE_WRITE],STDERR_FILENO)==-1){// redirect stderr
            perror("redirecting stderr");
            return;
        }
        close(StdinPipe[PIPE_READ]);
        close(StdinPipe[PIPE_WRITE]);
        close(StdoutPipe[PIPE_READ]);
        close(StdoutPipe[PIPE_WRITE]);
        close(StderrPipe[PIPE_READ]);
        close(StderrPipe[PIPE_WRITE]);
        execl(Bot.name.c_str(),Bot.name.c_str(),(char*)NULL);//(char*)Null is really important
        //If you get past the previous line its an error
        perror("exec of the child process");
    }
    else if(nchild>0){//Parent process
        close(StdinPipe[PIPE_READ]);//Parent does not read from stdin of child
        close(StdoutPipe[PIPE_WRITE]);//Parent does not write to stdout of child
        close(StderrPipe[PIPE_WRITE]);//Parent does not write to stderr of child
        Bot.inPipe=StdinPipe[PIPE_WRITE];
        Bot.outPipe=StdoutPipe[PIPE_READ];
        Bot.errPipe=StderrPipe[PIPE_READ];
        Bot.pid=nchild;
    }
    else{//failed to create child
        close(StdinPipe[PIPE_READ]);
        close(StdinPipe[PIPE_WRITE]);
        close(StdoutPipe[PIPE_READ]);
        close(StdoutPipe[PIPE_WRITE]);
        perror("Failed to create child process");
    }
}

inline bool IsValidMove(const string &M){
    return count(M.begin(),M.end(),'\n')==6;
}

string GetMove(const state &S,AI &Bot,const int turn){
    pollfd outpoll{Bot.outPipe,POLLIN};
    time_point<system_clock> Start_Time{system_clock::now()};
    string out;
    while(static_cast<duration<double>>(system_clock::now()-Start_Time).count()<(turn==1?FirstTurnTime:TimeLimit) && !IsValidMove(out)){
        double TimeLeft{(turn==1?FirstTurnTime:TimeLimit)-static_cast<duration<double>>(system_clock::now()-Start_Time).count()};
        if(poll(&outpoll,1,TimeLeft)){
            out+=EmptyPipe(Bot.outPipe);
        }
    }
    return out;
}

inline bool Has_Won(const array<AI,N> &Bot,const int idx)noexcept{
    if(!Bot[idx].alive()){
        return false;
    }
    for(int i=0;i<N;++i){
        if(i!=idx && Bot[i].alive()){
            return false;
        }
    }
    return true;
}

inline bool All_Dead(const array<AI,N> &Bot)noexcept{
    for(const AI &b:Bot){
        if(b.alive()){
            return false;
        }
    }
    return true;
}

action StringToAction(const state &S,const string &M_Str,const int playerId){
    action mv;
    if(!IsValidMove(M_Str)){
        throw(1);
    }
    stringstream ss(M_Str);
    for(int &affect_id:mv.affects){
        ss >>affect_id;
    }
    string spread_string;
    ss >> spread_string;
    if(spread_string=="NONE"){
        mv.spread=-1;
    }
    else{
        try{
           mv.spread=stoi(spread_string); 
        }
        catch(...){
            cerr << "Error: AI " << playerId << " requested spread of " << spread_string << endl;
            cerr << "Turn output:" << endl;
            cerr << M_Str << endl;
            throw(3);
        }
    }
    return mv;
}

int Play_Game(const array<string,N> &Bot_Names,state S){
    array<AI,N> Bot;
    for(int i=0;i<N;++i){
        Bot[i].id=i;
        Bot[i].name=Bot_Names[i];
        StartProcess(Bot[i]);
        stringstream ss;
        const int linkCount{accumulate(S.P.begin(),S.P.end(),0,[](int total,const planet &p){return total+static_cast<int>(p.L.size());})};
        if(linkCount%2!=0){
            cerr << "Error: Odd number of links" << endl;
        }
        ss << S.P.size() << " " << linkCount/2 << endl;
        for(int j=0;j<static_cast<int>(S.P.size());++j){
            for(int k=j+1;k<static_cast<int>(S.P.size());++k){//We have to avoid sending twice the same link
                if(find(S.P[j].L.begin(),S.P[j].L.end(),k)!=S.P[j].L.end()){
                    ss << j << " " << k << endl;
                }
            }
        }
        Bot[i].Feed_Inputs(ss.str());
    }
    int turn{0};
    while(++turn>0 && !stop){
        //cerr << "Turn: " << turn << endl;
        //cerr << S.to_string() << endl;
        array<action,2> M;
        for(int id=0;id<N;++id){
            if(Bot[id].alive()){
                stringstream ss;
                for(const planet &p:S.P){
                    for(int i=0;i<2;++i){
                        ss << p.units[(i+id)%2] << " " << p.tolerance[(i+id)%2] << " ";
                    }
                    ss << 1 << endl;//Should be a proper canAssign
                }
                //cerr << ss.str();
                try{
                    Bot[id].Feed_Inputs(ss.str());
                    string out=GetMove(S,Bot[id],turn);
                    //cerr << id << " " << out << endl;
                    string err_str{EmptyPipe(Bot[id].errPipe)};
                    if(Debug_AI){
                        ofstream err_out("log.txt",ios::app);
                        err_out << err_str << endl;
                    }
                    M[id]=StringToAction(S,out,id);
                }
                catch(int ex){
                    if(ex==1){//Timeout
                        cerr << "Loss by Timeout of AI " << Bot[id].id << " name: " << Bot[id].name << endl;
                    }
                    else if(ex==3){
                        cerr << "Invalid move from AI " << Bot[id].id << " name: " << Bot[id].name << endl;
                    }
                    else if(ex==4){
                        cerr << "Error emptying pipe of AI " << Bot[id].name << endl;
                    }
                    else if(ex==5){
                        cerr << "AI " << Bot[id].name << " died before being able to give it inputs" << endl;
                    }
                    Bot[id].stop(turn);
                    return (id+1)%2;
                }
            }
        }
        try{
           S.simulate(M); 
        }
        catch(int ex){
            if(ex==3){
                //Do nothing the following code will pick out the winner
            }
        }
        array<int,2> Planets{0,0};
        for(const planet &p:S.P){
            if(p.owner()!=-1){
                ++Planets[p.owner()];
            }
        }
        for(int i=0;i<2;++i){
            if(accumulate(S.P.begin(),S.P.end(),0,[&](const int total,const planet &p){return total+p.units[i];})==0){//No more units eliminated
                Bot[i].stop();
            }
        }
        for(int i=0;i<2;++i){
            if(Has_Won(Bot,i) || Planets[i]==static_cast<int>(S.P.size())){
                //cerr << "AI " << Bot_Names[i] << " has won in " << turn << " turns out of " << static_cast<int>(S.P.size()) << endl;
                return i;
            }
        }
        if(All_Dead(Bot) || turn==static_cast<int>(S.P.size())){
            //cerr << "Planets: " << Bot_Names[0] << ": " << Planets[0] << " " << Bot_Names[1] << " " << Planets[1] << endl;
            return Planets[0]>Planets[1]?0:Planets[1]>Planets[0]?1:-1;
        }
    }
    cerr << "Reached end of Play_Game function" << endl;
    throw(0);
}

int Play_Round(array<string,N> Bot_Names){
    default_random_engine generator(system_clock::now().time_since_epoch().count());
    uniform_int_distribution<int> Swap_Distrib(0,1);
    const bool player_swap{Swap_Distrib(generator)==1};
    //const bool player_swap{true};
    if(player_swap){
        swap(Bot_Names[0],Bot_Names[1]);
    }
    
    const vector<string> Map_Pool{"23 5 0 5 0 2 4 6 5 0 5 0 2 5 7 5 0 5 0 3 4 6 3 5 0 5 0 3 5 7 2 4 5 5 0 5 0 2 6 5 8 5 0 4 5 5 1 3 7 4 8 5 0 5 0 3 0 2 4 5 0 5 0 3 1 3 5 5 0 5 0 3 4 5 20 5 0 5 0 4 10 18 19 22 5 0 5 0 4 9 16 19 21 5 0 5 0 5 12 13 16 17 21 5 0 5 0 2 11 17 5 0 5 0 3 11 17 21 5 0 5 0 3 15 18 19 5 0 5 0 3 14 18 20 5 0 5 0 4 10 11 21 22 5 0 5 0 3 11 12 13 5 0 5 0 5 9 14 15 19 20 5 0 5 0 4 9 10 14 18 5 0 5 0 3 15 18 8 5 0 5 0 4 10 11 13 16 5 0 5 0 2 9 16",
    "38 5 0 5 0 5 2 12 26 28 35 5 0 5 0 5 3 13 27 29 34 5 0 5 0 3 0 11 17 5 0 5 0 3 1 10 16 5 0 5 0 5 7 11 13 14 19 5 0 5 0 5 6 10 12 15 18 5 0 5 0 4 5 15 21 22 5 0 5 0 4 4 14 20 23 5 0 5 0 3 20 23 27 5 0 5 0 3 21 22 26 5 0 5 0 5 3 5 18 25 31 5 0 5 0 5 2 4 19 24 30 5 0 5 0 4 0 5 18 26 5 0 5 0 4 1 4 19 27 5 0 5 0 3 4 7 30 5 0 5 0 3 5 6 31 5 0 5 0 3 3 32 31 5 0 5 0 3 2 33 30 5 0 5 0 4 5 10 12 19 5 0 5 0 4 4 11 13 18 5 0 5 0 3 7 8 23 5 0 5 0 3 6 9 22 5 0 5 0 4 6 9 21 26 5 0 5 0 4 7 8 20 27 5 0 5 0 3 11 35 37 5 0 5 0 3 10 34 36 5 0 5 0 4 0 9 12 22 5 0 5 0 4 1 8 13 23 5 0 5 0 2 0 33 5 0 5 0 2 1 32 5 0 5 0 3 11 14 17 5 0 5 0 3 10 15 16 5 0 5 0 2 16 29 5 0 5 0 2 17 28 5 0 4 5 3 1 25 36 4 5 5 0 3 0 24 37 5 0 5 0 2 25 34 5 0 5 0 2 24 35",
    "34 5 0 5 0 2 6 10 5 0 5 0 2 7 11 5 0 5 0 5 4 6 8 10 12 5 0 5 0 5 5 7 9 11 13 5 0 4 5 3 2 10 12 4 5 5 0 3 3 11 13 5 0 5 0 5 0 2 8 7 14 5 0 5 0 5 1 3 9 6 14 5 0 5 0 3 2 6 12 5 0 5 0 3 3 7 13 5 0 5 0 3 0 2 4 5 0 5 0 3 1 3 5 5 0 5 0 3 2 4 8 5 0 5 0 3 3 5 9 5 0 5 0 3 6 7 27 5 0 5 0 4 16 27 28 30 5 0 5 0 5 15 19 22 30 31 5 0 5 0 4 21 24 29 32 5 0 5 0 5 19 22 23 25 26 5 0 5 0 3 16 18 22 5 0 5 0 5 23 24 26 32 33 5 0 5 0 3 17 29 32 5 0 5 0 5 16 18 19 25 31 5 0 5 0 5 18 20 25 26 33 5 0 5 0 4 17 20 32 33 5 0 5 0 3 18 22 23 5 0 5 0 3 18 20 23 5 0 5 0 4 15 28 31 14 5 0 5 0 3 15 27 30 5 0 5 0 2 17 21 5 0 5 0 3 15 16 28 5 0 5 0 3 16 22 27 5 0 5 0 4 17 20 21 24 5 0 5 0 3 20 23 24",
    "36 5 0 5 0 7 3 6 11 18 23 27 28 5 0 5 0 7 2 7 10 19 22 26 29 5 0 5 0 5 1 7 14 22 30 5 0 5 0 5 0 6 15 23 31 5 0 5 0 5 14 16 30 32 35 5 0 5 0 5 15 17 31 33 34 5 0 5 0 5 0 3 11 12 15 5 0 5 0 5 1 2 10 13 14 5 0 5 0 3 16 26 30 5 0 5 0 3 17 27 31 5 0 5 0 6 1 7 13 20 25 19 5 0 5 0 6 0 6 12 21 24 18 5 0 5 0 4 6 11 32 35 5 0 5 0 4 7 10 33 34 4 5 5 0 3 2 4 7 5 0 4 5 3 3 5 6 5 0 5 0 3 4 8 30 5 0 5 0 3 5 9 31 5 0 5 0 4 0 11 24 28 5 0 5 0 4 1 10 25 29 5 0 5 0 2 10 34 5 0 5 0 2 11 35 5 0 5 0 4 1 2 26 30 5 0 5 0 4 0 3 27 31 5 0 5 0 2 11 18 5 0 5 0 2 10 19 5 0 5 0 4 1 8 22 29 5 0 5 0 4 0 9 23 28 5 0 5 0 3 0 18 27 5 0 5 0 3 1 19 26 5 0 5 0 5 2 4 8 16 22 5 0 5 0 5 3 5 9 17 23 5 0 5 0 4 4 12 33 35 5 0 5 0 4 5 13 32 34 5 0 5 0 4 5 13 20 33 5 0 5 0 4 4 12 21 32",
    "32 5 0 5 0 3 4 10 14 5 0 5 0 3 5 11 15 5 0 5 0 4 4 6 10 12 5 0 5 0 4 5 7 11 13 5 0 5 0 4 0 2 10 14 5 0 5 0 4 1 3 11 15 5 0 5 0 3 2 8 12 5 0 5 0 3 3 9 13 5 0 5 0 3 6 12 16 5 0 5 0 3 7 13 16 5 0 4 5 3 0 2 4 4 5 5 0 3 1 3 5 5 0 5 0 4 2 6 8 13 5 0 5 0 4 3 7 9 12 5 0 5 0 2 0 4 5 0 5 0 2 1 5 5 0 5 0 3 8 9 20 5 0 5 0 3 19 22 26 5 0 5 0 5 19 24 25 26 30 5 0 5 0 3 17 18 22 5 0 5 0 4 23 28 29 16 5 0 5 0 3 25 30 31 5 0 5 0 5 17 19 24 27 29 5 0 5 0 4 20 27 28 29 5 0 5 0 4 18 22 29 30 5 0 5 0 4 18 21 26 30 5 0 5 0 3 17 18 25 5 0 5 0 3 22 23 29 5 0 5 0 2 20 23 5 0 5 0 5 20 22 23 24 27 5 0 5 0 5 18 21 24 25 31 5 0 5 0 2 21 30",
    "65 5 0 5 0 5 2 6 8 20 26 5 0 5 0 5 3 7 9 21 27 4 5 5 0 5 0 16 20 22 26 5 0 4 5 5 1 17 21 23 27 5 0 5 0 6 6 14 18 24 26 28 5 0 5 0 6 7 15 19 25 27 29 5 0 5 0 3 0 4 8 5 0 5 0 3 1 5 9 5 0 5 0 4 0 6 12 30 5 0 5 0 4 1 7 13 30 5 0 5 0 3 11 14 28 5 0 5 0 3 10 15 29 5 0 5 0 4 8 13 14 24 5 0 5 0 4 9 12 15 25 5 0 5 0 3 4 10 12 5 0 5 0 3 5 11 13 5 0 5 0 3 2 18 22 5 0 5 0 3 3 19 23 5 0 5 0 4 4 16 26 28 5 0 5 0 4 5 17 27 29 5 0 5 0 3 0 2 22 5 0 5 0 3 1 3 23 5 0 5 0 3 2 16 20 5 0 5 0 3 3 17 21 5 0 5 0 2 4 12 5 0 5 0 2 5 13 5 0 5 0 4 0 2 4 18 5 0 5 0 4 1 3 5 19 5 0 5 0 3 4 10 18 5 0 5 0 3 5 11 19 5 0 5 0 3 9 8 43 5 0 5 0 5 34 40 42 43 46 5 0 5 0 3 39 51 54 5 0 5 0 4 38 47 48 49 5 0 5 0 4 31 35 40 43 5 0 5 0 4 34 40 47 48 5 0 5 0 5 50 51 54 60 63 5 0 5 0 5 41 44 52 58 59 5 0 5 0 6 33 48 49 53 57 62 5 0 5 0 3 32 50 54 5 0 5 0 4 31 34 35 42 5 0 5 0 5 37 44 45 52 53 5 0 5 0 5 31 40 46 56 64 5 0 5 0 5 31 34 50 63 30 5 0 5 0 4 37 41 53 57 5 0 5 0 3 41 52 62 5 0 5 0 3 31 42 56 5 0 5 0 4 33 35 48 64 5 0 5 0 4 33 35 38 47 5 0 5 0 5 33 38 55 57 64 5 0 5 0 4 36 39 43 63 5 0 5 0 3 32 36 60 5 0 5 0 4 37 41 45 58 5 0 5 0 3 38 41 44 5 0 5 0 3 32 36 39 5 0 5 0 3 49 56 57 5 0 5 0 3 42 46 55 5 0 5 0 4 38 44 49 55 5 0 5 0 3 37 52 61 5 0 5 0 2 37 61 5 0 5 0 3 36 51 63 5 0 5 0 2 58 59 5 0 5 0 2 38 45 5 0 5 0 4 36 43 50 60 5 0 5 0 3 42 47 49",
    "40 5 0 5 0 4 2 4 11 23 5 0 5 0 4 3 5 10 22 5 0 5 0 4 0 11 12 19 5 0 5 0 4 1 10 13 18 5 0 5 0 6 0 13 24 27 30 34 5 0 5 0 6 1 12 25 26 31 35 5 0 5 0 3 8 22 32 5 0 5 0 3 9 23 33 5 0 5 0 3 6 21 26 5 0 5 0 3 7 20 27 5 0 5 0 6 1 3 18 22 32 37 5 0 5 0 6 0 2 19 23 33 36 5 0 5 0 8 2 5 13 15 19 25 29 31 5 0 5 0 8 3 4 12 14 18 24 28 30 5 0 5 0 4 13 16 28 30 5 0 5 0 4 12 17 29 31 5 0 5 0 3 14 30 34 5 0 5 0 3 15 31 35 5 0 5 0 5 3 10 13 28 37 5 0 5 0 5 2 11 12 29 36 5 0 5 0 3 9 27 34 5 0 5 0 3 8 26 35 5 0 5 0 4 1 6 10 38 5 0 5 0 4 0 7 11 39 5 0 5 0 2 4 13 5 0 5 0 2 5 12 5 0 5 0 5 5 8 21 35 38 5 0 5 0 5 4 9 20 34 39 4 5 5 0 4 13 14 18 37 5 0 4 5 4 12 15 19 36 5 0 5 0 4 4 13 14 16 5 0 5 0 4 5 12 15 17 5 0 5 0 3 6 10 37 5 0 5 0 3 7 11 36 5 0 5 0 4 4 16 20 27 5 0 5 0 4 5 17 21 26 5 0 5 0 4 11 19 29 33 5 0 5 0 4 10 18 28 32 5 0 5 0 2 22 26 5 0 5 0 2 23 27",
    "34 5 0 4 5 6 9 10 11 15 16 21 4 5 5 0 6 8 11 10 14 17 20 5 0 5 0 6 5 8 22 26 28 32 5 0 5 0 6 4 9 23 27 29 33 5 0 5 0 3 3 9 12 5 0 5 0 3 2 8 13 5 0 5 0 6 15 16 19 21 22 26 5 0 5 0 6 14 17 18 20 23 27 5 0 5 0 3 1 2 5 5 0 5 0 3 0 3 4 5 0 5 0 3 0 1 15 5 0 5 0 3 1 0 14 5 0 5 0 3 4 21 24 5 0 5 0 3 5 20 25 5 0 5 0 4 1 7 11 17 5 0 5 0 4 0 6 10 16 5 0 5 0 3 0 6 15 5 0 5 0 3 1 7 14 5 0 5 0 4 7 30 33 23 5 0 5 0 4 6 31 32 22 5 0 5 0 3 1 7 13 5 0 5 0 3 0 6 12 5 0 5 0 5 2 6 19 26 32 5 0 5 0 5 3 7 18 27 33 5 0 5 0 1 12 5 0 5 0 1 13 5 0 5 0 3 2 6 22 5 0 5 0 3 3 7 23 5 0 5 0 1 2 5 0 5 0 1 3 5 0 5 0 1 18 5 0 5 0 1 19 5 0 5 0 3 2 19 22 5 0 5 0 3 3 18 23",
    "40 5 0 5 0 6 3 13 18 22 25 31 5 0 5 0 6 2 12 19 23 24 30 5 0 5 0 3 1 16 23 5 0 5 0 3 0 17 22 5 0 5 0 4 11 17 26 39 5 0 5 0 4 10 16 27 38 5 0 5 0 5 9 20 29 30 32 5 0 5 0 5 8 21 28 31 33 5 0 5 0 2 7 28 5 0 5 0 2 6 29 5 0 5 0 5 5 36 38 23 27 5 0 5 0 5 4 37 39 22 26 5 0 5 0 3 1 24 34 5 0 5 0 3 0 25 35 5 0 5 0 6 22 25 29 30 35 37 5 0 5 0 6 23 24 28 31 34 36 5 0 5 0 3 2 5 19 5 0 5 0 3 3 4 18 5 0 5 0 2 0 17 5 0 5 0 2 1 16 5 0 5 0 2 6 32 5 0 5 0 2 7 33 5 0 5 0 7 0 3 11 14 25 26 37 5 0 5 0 7 1 2 10 15 24 27 36 5 0 4 5 5 1 12 15 23 34 4 5 5 0 5 0 13 14 22 35 5 0 5 0 3 4 11 22 5 0 5 0 3 5 10 23 5 0 5 0 4 7 8 15 31 5 0 5 0 4 6 9 14 30 5 0 5 0 6 1 6 14 29 32 35 5 0 5 0 6 0 7 15 28 33 34 5 0 5 0 3 6 20 30 5 0 5 0 3 7 21 31 5 0 5 0 5 12 15 24 31 35 5 0 5 0 5 13 14 25 30 34 5 0 5 0 3 10 15 23 5 0 5 0 3 11 14 22 5 0 5 0 2 5 10 5 0 5 0 2 4 11",
    "38 5 0 5 0 3 6 10 14 5 0 5 0 3 7 11 15 5 0 4 5 5 4 6 8 12 14 4 5 5 0 5 5 7 9 13 15 5 0 5 0 4 2 10 12 14 5 0 5 0 4 3 11 13 15 5 0 5 0 4 0 2 8 16 5 0 5 0 4 1 3 9 16 5 0 5 0 2 2 6 5 0 5 0 2 3 7 5 0 5 0 3 0 4 14 5 0 5 0 3 1 5 15 5 0 5 0 2 2 4 5 0 5 0 2 3 5 5 0 5 0 4 0 2 4 10 5 0 5 0 4 1 3 5 11 5 0 5 0 3 7 6 31 5 0 5 0 5 19 20 28 34 37 5 0 5 0 4 22 24 27 29 5 0 5 0 4 17 28 30 34 5 0 5 0 3 17 22 28 5 0 5 0 3 25 32 35 5 0 5 0 4 18 20 24 27 5 0 5 0 6 25 26 30 34 35 36 5 0 5 0 3 18 22 33 5 0 5 0 5 21 23 32 35 36 5 0 5 0 3 23 34 35 5 0 5 0 4 18 22 29 31 5 0 5 0 3 17 19 20 5 0 5 0 3 18 27 33 5 0 5 0 3 19 23 34 5 0 5 0 2 27 16 5 0 5 0 3 21 25 36 5 0 5 0 2 24 29 5 0 5 0 6 17 19 23 26 30 37 5 0 5 0 5 21 23 25 26 37 5 0 5 0 3 23 25 32 5 0 5 0 3 17 34 35",
    "81 4 5 5 0 2 1 2 5 0 5 0 4 0 2 3 4 5 0 5 0 4 0 1 4 5 5 0 5 0 4 1 4 6 7 5 0 5 0 6 1 2 3 5 7 8 5 0 5 0 4 2 4 8 9 5 0 5 0 4 3 7 10 11 5 0 5 0 6 3 4 6 8 11 12 5 0 5 0 6 4 5 7 9 12 13 5 0 5 0 4 5 8 13 14 5 0 5 0 4 6 11 15 16 5 0 5 0 6 6 7 10 12 16 17 5 0 5 0 6 7 8 11 13 17 18 5 0 5 0 6 8 9 12 14 18 19 5 0 5 0 4 9 13 19 20 5 0 5 0 4 10 16 21 22 5 0 5 0 5 10 11 15 17 22 5 0 5 0 5 11 12 16 18 23 5 0 5 0 6 12 13 17 19 23 24 5 0 5 0 6 13 14 18 20 24 25 5 0 5 0 4 14 19 25 26 5 0 5 0 4 15 22 28 29 5 0 5 0 4 15 16 21 29 5 0 5 0 4 17 18 24 30 5 0 5 0 6 18 19 23 25 30 31 5 0 5 0 6 19 20 24 26 31 32 5 0 5 0 4 20 25 32 33 5 0 5 0 2 34 35 5 0 5 0 4 21 29 36 37 5 0 5 0 4 21 22 28 37 5 0 5 0 4 23 24 31 38 5 0 5 0 6 24 25 30 32 38 39 5 0 5 0 6 25 26 31 33 39 40 5 0 5 0 4 26 32 40 41 5 0 5 0 4 27 35 42 43 5 0 5 0 4 27 34 43 44 5 0 5 0 3 28 37 45 5 0 5 0 5 28 29 36 45 46 5 0 5 0 5 30 31 39 47 48 5 0 5 0 6 31 32 38 40 48 49 5 0 5 0 6 32 33 39 41 49 50 5 0 5 0 4 33 40 42 50 5 0 5 0 4 34 41 43 51 5 0 5 0 6 34 35 42 44 51 52 5 0 5 0 3 35 43 52 5 0 5 0 4 36 37 46 53 5 0 5 0 4 37 45 53 54 5 0 5 0 4 38 48 55 56 5 0 5 0 6 38 39 47 49 56 57 5 0 5 0 6 39 40 48 50 57 58 5 0 5 0 4 40 41 49 58 5 0 5 0 4 42 43 52 59 5 0 5 0 4 43 44 51 59 5 0 5 0 4 45 46 54 60 5 0 5 0 4 46 53 60 61 5 0 5 0 4 47 56 62 63 5 0 5 0 6 47 48 55 57 63 64 5 0 5 0 6 48 49 56 58 64 65 5 0 5 0 4 49 50 57 65 5 0 5 0 2 51 52 5 0 5 0 4 53 54 61 66 5 0 5 0 5 54 60 62 66 67 5 0 5 0 5 55 61 63 67 68 5 0 5 0 6 55 56 62 64 68 69 5 0 5 0 6 56 57 63 65 69 70 5 0 5 0 4 57 58 64 70 5 0 5 0 4 60 61 67 71 5 0 5 0 6 61 62 66 68 71 72 5 0 5 0 6 62 63 67 69 72 73 5 0 5 0 6 63 64 68 70 73 74 5 0 5 0 4 64 65 69 74 5 0 5 0 4 66 67 72 75 5 0 5 0 6 67 68 71 73 75 76 5 0 5 0 6 68 69 72 74 76 77 5 0 5 0 4 69 70 73 77 5 0 5 0 4 71 72 76 78 5 0 5 0 6 72 73 75 77 78 79 5 0 5 0 4 73 74 76 79 5 0 5 0 4 75 76 79 80 5 0 5 0 4 76 77 78 80 5 0 4 5 2 78 79",
    "31 5 0 4 5 4 4 8 10 12 4 5 5 0 4 5 9 11 13 5 0 5 0 3 4 6 12 5 0 5 0 3 5 7 13 5 0 5 0 5 0 2 6 10 12 5 0 5 0 5 1 3 7 11 13 5 0 5 0 4 2 4 7 10 5 0 5 0 4 3 5 6 11 5 0 5 0 1 0 5 0 5 0 1 1 5 0 5 0 4 0 4 6 14 5 0 5 0 4 1 5 7 14 5 0 5 0 3 0 2 4 5 0 5 0 3 1 3 5 5 0 5 0 3 10 11 16 5 0 5 0 3 17 19 21 5 0 5 0 4 17 19 25 14 5 0 5 0 7 15 16 18 21 24 25 27 5 0 5 0 7 17 20 22 24 26 28 30 5 0 5 0 4 15 16 27 29 5 0 5 0 3 18 22 24 5 0 5 0 3 15 17 24 5 0 5 0 3 18 20 28 5 0 5 0 1 29 5 0 5 0 4 17 18 20 21 5 0 5 0 3 16 17 30 5 0 5 0 3 18 28 30 5 0 5 0 2 17 19 5 0 5 0 3 18 22 26 5 0 5 0 2 19 23 5 0 5 0 3 18 25 26",
    "38 5 0 5 0 4 4 6 13 19 5 0 5 0 4 5 7 12 18 5 0 5 0 7 10 12 17 18 23 30 34 5 0 5 0 7 11 13 16 19 22 31 35 5 0 5 0 3 0 6 16 5 0 5 0 3 1 7 17 5 0 5 0 5 0 4 15 26 33 5 0 5 0 5 1 5 14 27 32 5 0 5 0 3 21 25 30 5 0 5 0 3 20 24 31 5 0 5 0 7 2 11 13 22 23 25 29 5 0 5 0 7 3 10 12 23 22 24 28 5 0 5 0 6 1 2 11 23 28 18 5 0 5 0 6 0 3 10 22 29 19 5 0 5 0 4 7 17 32 36 5 0 5 0 4 6 16 33 37 5 0 4 5 7 3 4 15 35 37 19 33 4 5 5 0 7 2 5 14 34 36 18 32 5 0 5 0 4 1 2 12 17 5 0 5 0 4 0 3 13 16 5 0 5 0 3 9 24 28 5 0 5 0 3 8 25 29 5 0 5 0 4 3 10 11 13 5 0 5 0 4 2 11 10 12 5 0 5 0 4 9 11 20 28 5 0 5 0 4 8 10 21 29 5 0 5 0 1 6 5 0 5 0 1 7 5 0 5 0 4 11 12 20 24 5 0 5 0 4 10 13 21 25 5 0 5 0 3 2 8 34 5 0 5 0 3 3 9 35 5 0 5 0 3 7 14 17 5 0 5 0 3 6 15 16 5 0 5 0 4 2 17 30 36 5 0 5 0 4 3 16 31 37 5 0 5 0 3 14 17 34 5 0 5 0 3 15 16 35",
    "36 5 0 5 0 5 11 19 20 28 35 5 0 5 0 5 10 18 21 29 34 5 0 5 0 4 8 15 30 33 5 0 5 0 4 9 14 31 32 5 0 5 0 6 7 12 17 18 22 31 5 0 5 0 6 6 13 16 19 23 30 5 0 5 0 6 5 15 16 21 26 30 5 0 5 0 6 4 14 17 20 27 31 5 0 5 0 3 2 13 33 5 0 5 0 3 3 12 32 5 0 5 0 6 1 22 24 25 34 18 5 0 5 0 6 0 23 25 24 35 19 5 0 5 0 4 4 9 18 29 5 0 5 0 4 5 8 19 28 5 0 5 0 3 3 7 31 5 0 5 0 3 2 6 30 5 0 4 5 3 5 6 23 4 5 5 0 3 4 7 22 5 0 5 0 5 1 4 10 12 29 5 0 5 0 5 0 5 11 13 28 5 0 5 0 3 0 7 27 5 0 5 0 3 1 6 26 5 0 5 0 3 4 10 17 5 0 5 0 3 5 11 16 5 0 5 0 5 10 11 25 26 34 5 0 5 0 5 11 10 24 27 35 5 0 5 0 4 6 21 24 34 5 0 5 0 4 7 20 25 35 5 0 5 0 3 0 13 19 5 0 5 0 3 1 12 18 5 0 5 0 5 2 5 6 15 33 5 0 5 0 5 3 4 7 14 32 5 0 5 0 3 3 9 31 5 0 5 0 3 2 8 30 5 0 5 0 4 1 10 24 26 5 0 5 0 4 0 11 25 27",
    "34 5 0 5 0 8 1 3 10 11 12 18 24 29 5 0 5 0 8 0 2 11 10 13 19 25 28 5 0 5 0 4 1 8 16 19 5 0 5 0 4 0 9 17 18 5 0 5 0 6 6 13 14 23 28 30 5 0 5 0 6 7 12 15 22 29 31 5 0 5 0 6 4 14 27 30 32 9 5 0 5 0 6 5 15 26 31 33 8 5 0 5 0 5 2 7 11 15 25 5 0 5 0 5 3 6 10 14 24 5 0 5 0 5 0 1 9 24 14 5 0 5 0 5 1 0 8 25 15 5 0 5 0 5 0 5 18 20 29 5 0 5 0 5 1 4 19 21 28 5 0 5 0 4 4 6 9 10 5 0 5 0 4 5 7 8 11 5 0 5 0 3 2 21 33 5 0 5 0 3 3 20 32 5 0 5 0 3 0 3 12 5 0 5 0 3 1 2 13 5 0 4 5 2 12 17 4 5 5 0 2 13 16 5 0 5 0 2 5 31 5 0 5 0 2 4 30 5 0 5 0 3 0 9 10 5 0 5 0 3 1 8 11 5 0 5 0 2 7 33 5 0 5 0 2 6 32 5 0 5 0 3 1 4 13 5 0 5 0 3 0 5 12 5 0 5 0 3 4 6 23 5 0 5 0 3 5 7 22 5 0 5 0 3 6 17 27 5 0 5 0 3 7 16 26",
    "37 5 0 5 0 5 2 4 6 8 10 5 0 5 0 5 3 5 7 9 11 5 0 5 0 2 0 6 5 0 5 0 2 1 7 5 0 5 0 5 0 8 10 12 16 5 0 5 0 5 1 9 11 13 17 5 0 5 0 4 0 2 7 10 5 0 5 0 4 1 3 6 11 5 0 4 5 4 0 4 12 18 4 5 5 0 4 1 5 13 18 5 0 5 0 4 0 4 6 16 5 0 5 0 4 1 5 7 17 5 0 5 0 4 4 8 14 16 5 0 5 0 4 5 9 15 17 5 0 5 0 2 12 16 5 0 5 0 2 13 17 5 0 5 0 4 4 10 12 14 5 0 5 0 4 5 11 13 15 5 0 5 0 3 8 9 28 5 0 5 0 5 28 29 30 32 36 5 0 5 0 3 21 24 26 5 0 5 0 4 20 22 24 29 5 0 5 0 5 21 29 30 33 36 5 0 5 0 2 25 34 5 0 5 0 3 20 21 33 5 0 5 0 3 23 28 30 5 0 5 0 4 20 27 31 35 5 0 5 0 1 26 5 0 5 0 5 19 25 32 34 18 5 0 5 0 5 19 21 22 31 36 5 0 5 0 4 19 22 25 32 5 0 5 0 2 26 29 5 0 5 0 3 19 28 30 5 0 5 0 2 22 24 5 0 5 0 2 23 28 5 0 5 0 1 26 5 0 5 0 3 19 22 29",
    "40 5 0 5 0 4 8 11 17 32 5 0 5 0 4 9 10 16 33 5 0 5 0 5 13 22 25 36 39 5 0 5 0 5 12 23 24 37 38 5 0 5 0 4 14 21 31 38 5 0 5 0 4 15 20 30 39 5 0 5 0 5 16 17 25 33 36 5 0 5 0 5 17 16 24 32 37 5 0 5 0 3 0 19 26 5 0 5 0 3 1 18 27 5 0 4 5 5 1 12 16 35 37 4 5 5 0 5 0 13 17 34 36 5 0 5 0 6 3 10 18 35 37 28 5 0 5 0 6 2 11 19 34 36 29 5 0 5 0 3 4 21 31 5 0 5 0 3 5 20 30 5 0 5 0 6 1 6 7 10 17 33 5 0 5 0 6 0 7 6 11 16 32 5 0 5 0 4 9 12 28 35 5 0 5 0 4 8 13 29 34 5 0 5 0 4 5 15 22 39 5 0 5 0 4 4 14 23 38 5 0 5 0 3 2 20 39 5 0 5 0 3 3 21 38 5 0 5 0 3 3 7 31 5 0 5 0 3 2 6 30 5 0 5 0 1 8 5 0 5 0 1 9 5 0 5 0 2 12 18 5 0 5 0 2 13 19 5 0 5 0 3 5 15 25 5 0 5 0 3 4 14 24 5 0 5 0 3 0 7 17 5 0 5 0 3 1 6 16 5 0 5 0 3 11 13 19 5 0 5 0 3 10 12 18 5 0 5 0 4 2 6 11 13 5 0 5 0 4 3 7 10 12 5 0 5 0 4 3 4 21 23 5 0 5 0 4 2 5 20 22",
    "26 5 0 5 0 4 2 4 6 8 5 0 5 0 4 3 5 7 9 5 0 5 0 4 0 6 8 10 5 0 5 0 4 1 7 9 10 5 0 5 0 2 0 6 5 0 5 0 2 1 7 5 0 5 0 3 0 2 4 5 0 5 0 3 1 3 5 4 5 5 0 3 0 2 9 5 0 4 5 3 1 3 8 5 0 5 0 3 2 3 16 5 0 5 0 6 12 13 16 17 22 23 5 0 5 0 5 11 15 16 17 20 5 0 5 0 3 11 16 22 5 0 5 0 4 19 21 22 24 5 0 5 0 3 12 18 20 5 0 5 0 4 11 12 13 10 5 0 5 0 4 11 12 20 23 5 0 5 0 2 15 20 5 0 5 0 4 14 21 22 25 5 0 5 0 4 12 15 17 18 5 0 5 0 3 14 19 24 5 0 5 0 4 11 13 14 19 5 0 5 0 3 11 17 25 5 0 5 0 2 14 21 5 0 5 0 2 19 23",
    "22 5 0 5 0 2 2 8 5 0 5 0 2 3 9 5 0 5 0 3 0 4 6 5 0 5 0 3 1 5 7 5 0 5 0 4 2 6 8 10 5 0 5 0 4 3 7 9 11 5 0 5 0 4 2 4 7 10 5 0 5 0 4 3 5 6 11 5 0 5 0 3 0 4 10 5 0 5 0 3 1 5 11 5 0 4 5 4 4 6 8 12 4 5 5 0 4 5 7 9 12 5 0 5 0 3 10 11 16 5 0 5 0 1 18 5 0 5 0 4 15 17 20 21 5 0 5 0 3 14 17 18 5 0 5 0 2 19 12 5 0 5 0 4 14 15 19 21 5 0 5 0 3 13 15 20 5 0 5 0 3 16 17 21 5 0 5 0 2 14 18 5 0 5 0 3 14 17 19",
    "42 5 0 5 0 6 7 8 10 17 20 33 5 0 5 0 6 6 9 11 16 21 32 5 0 5 0 3 9 12 19 5 0 5 0 3 8 13 18 5 0 5 0 4 5 6 7 21 5 0 5 0 4 4 7 6 20 5 0 5 0 5 1 4 5 22 32 5 0 5 0 5 0 5 4 23 33 5 0 5 0 4 0 3 13 15 5 0 5 0 4 1 2 12 14 5 0 5 0 4 0 17 29 36 5 0 5 0 4 1 16 28 37 5 0 5 0 4 2 9 40 14 5 0 5 0 4 3 8 41 15 5 0 5 0 4 9 12 28 37 5 0 5 0 4 8 13 29 36 5 0 5 0 3 1 11 24 5 0 5 0 3 0 10 25 5 0 5 0 7 3 20 22 24 27 31 35 5 0 5 0 7 2 21 23 25 26 30 34 5 0 5 0 4 0 5 18 38 5 0 5 0 4 1 4 19 39 5 0 5 0 5 6 18 24 32 38 5 0 5 0 5 7 19 25 33 39 5 0 5 0 3 16 18 22 5 0 5 0 3 17 19 23 5 0 5 0 3 19 30 34 5 0 5 0 3 18 31 35 5 0 5 0 3 11 14 37 5 0 5 0 3 10 15 36 5 0 5 0 2 19 26 5 0 5 0 2 18 27 5 0 5 0 3 1 6 22 5 0 5 0 3 0 7 23 5 0 4 5 3 19 26 40 4 5 5 0 3 18 27 41 5 0 5 0 3 10 15 29 5 0 5 0 3 11 14 28 5 0 5 0 2 20 22 5 0 5 0 2 21 23 5 0 5 0 2 12 34 5 0 5 0 2 13 35",
    "36 5 0 5 0 3 19 23 27 5 0 5 0 3 18 22 26 5 0 5 0 5 7 8 21 23 35 5 0 5 0 5 6 9 20 22 34 5 0 5 0 5 6 11 12 15 16 5 0 5 0 5 7 10 13 14 17 5 0 5 0 8 3 4 12 16 20 18 22 30 5 0 5 0 8 2 5 13 17 21 19 23 31 5 0 5 0 2 2 35 5 0 5 0 2 3 34 5 0 5 0 4 5 14 25 28 5 0 5 0 4 4 15 24 29 5 0 5 0 3 4 6 27 5 0 5 0 3 5 7 26 5 0 5 0 5 5 10 26 28 34 5 0 5 0 5 4 11 27 29 35 5 0 5 0 4 4 6 24 32 5 0 5 0 4 5 7 25 33 5 0 5 0 5 1 6 19 22 30 5 0 5 0 5 0 7 18 23 31 5 0 5 0 2 3 6 5 0 5 0 2 2 7 5 0 4 5 5 1 3 6 18 34 4 5 5 0 5 0 2 7 19 35 5 0 5 0 3 11 16 32 5 0 5 0 3 10 17 33 5 0 5 0 3 1 13 14 5 0 5 0 3 0 12 15 5 0 5 0 2 10 14 5 0 5 0 2 11 15 5 0 5 0 2 6 18 5 0 5 0 2 7 19 5 0 5 0 2 16 24 5 0 5 0 2 17 25 5 0 5 0 4 3 9 14 22 5 0 5 0 4 2 8 15 23",
    "48 5 0 5 0 4 1 4 18 24 5 0 5 0 4 0 5 19 25 5 0 5 0 4 14 16 18 24 5 0 5 0 4 15 17 19 25 5 0 5 0 4 0 5 8 18 5 0 5 0 4 1 4 9 19 5 0 5 0 5 8 12 14 16 18 5 0 5 0 5 9 13 15 17 19 5 0 5 0 4 4 6 18 22 5 0 5 0 4 5 7 19 23 5 0 5 0 2 14 20 5 0 5 0 2 15 21 5 0 5 0 3 6 20 22 5 0 5 0 3 7 21 23 5 0 5 0 5 2 6 10 16 20 5 0 5 0 5 3 7 11 17 21 5 0 5 0 4 2 6 14 18 5 0 5 0 4 3 7 15 19 4 5 5 0 6 0 2 4 6 8 16 5 0 4 5 6 1 3 5 7 9 17 5 0 5 0 3 10 12 14 5 0 5 0 3 11 13 15 5 0 5 0 3 8 12 26 5 0 5 0 3 9 13 26 5 0 5 0 3 0 2 25 5 0 5 0 3 1 3 24 5 0 5 0 3 22 23 44 5 0 5 0 3 29 30 36 5 0 5 0 4 31 36 37 41 5 0 5 0 5 27 30 40 45 47 5 0 5 0 3 27 29 33 5 0 5 0 5 28 36 37 44 45 5 0 5 0 3 37 42 44 5 0 5 0 5 30 34 35 43 47 5 0 5 0 3 33 39 46 5 0 5 0 4 33 39 40 43 5 0 5 0 5 27 28 31 41 45 5 0 5 0 4 28 31 32 44 5 0 5 0 1 41 5 0 5 0 3 34 35 43 5 0 5 0 3 29 35 47 5 0 5 0 3 28 36 38 5 0 5 0 2 32 44 5 0 5 0 3 33 35 39 5 0 5 0 5 31 32 37 42 26 5 0 5 0 3 29 31 36 5 0 5 0 1 34 5 0 5 0 3 29 33 40",
    "21 5 0 5 0 3 2 6 8 5 0 5 0 3 3 7 9 5 0 5 0 5 0 4 6 8 10 5 0 5 0 5 1 5 7 9 10 5 0 5 0 3 2 5 8 5 0 5 0 3 3 4 9 5 0 5 0 2 0 2 5 0 5 0 2 1 3 5 0 4 5 3 0 2 4 4 5 5 0 3 1 3 5 5 0 5 0 3 2 3 11 5 0 5 0 4 15 18 20 10 5 0 5 0 5 14 15 16 17 19 5 0 5 0 2 16 17 5 0 5 0 3 12 15 18 5 0 5 0 3 11 12 14 5 0 5 0 4 12 13 17 19 5 0 5 0 3 12 13 16 5 0 5 0 2 11 14 5 0 5 0 2 12 16 5 0 5 0 1 11",
    "36 5 0 5 0 3 2 5 9 5 0 5 0 3 3 4 8 5 0 5 0 8 0 5 7 13 19 22 23 34 5 0 5 0 8 1 4 6 12 18 23 22 35 5 0 5 0 5 1 3 8 22 24 5 0 5 0 5 0 2 9 23 25 5 0 5 0 5 3 10 28 15 35 5 0 5 0 5 2 11 29 14 34 5 0 5 0 7 1 4 19 21 24 27 30 5 0 5 0 7 0 5 18 20 25 26 31 5 0 5 0 4 6 26 28 18 5 0 5 0 4 7 27 29 19 5 0 5 0 4 3 16 32 35 5 0 5 0 4 2 17 33 34 5 0 5 0 4 7 33 34 29 5 0 5 0 4 6 32 35 28 5 0 5 0 3 12 21 32 5 0 5 0 3 13 20 33 5 0 5 0 5 3 9 10 25 26 5 0 5 0 5 2 8 11 24 27 5 0 5 0 3 9 17 31 5 0 5 0 3 8 16 30 5 0 5 0 5 2 3 4 23 24 5 0 5 0 5 3 2 5 22 25 5 0 5 0 4 4 8 19 22 5 0 5 0 4 5 9 18 23 5 0 5 0 4 9 10 18 31 5 0 5 0 4 8 11 19 30 5 0 5 0 3 6 10 15 5 0 5 0 3 7 11 14 5 0 5 0 3 8 21 27 5 0 5 0 3 9 20 26 5 0 5 0 3 12 15 16 5 0 5 0 3 13 14 17 4 5 5 0 4 2 7 13 14 5 0 4 5 4 3 6 12 15",
    "17 5 0 5 0 3 2 4 1 5 0 5 0 3 3 5 0 5 0 5 0 3 0 4 3 5 0 5 0 3 1 5 2 4 5 5 0 3 0 2 6 5 0 4 5 3 1 3 6 5 0 5 0 3 4 5 15 5 0 5 0 3 8 11 15 5 0 5 0 4 7 12 14 15 5 0 5 0 2 10 13 5 0 5 0 4 9 12 13 14 5 0 5 0 2 7 15 5 0 5 0 4 8 10 14 16 5 0 5 0 3 9 10 16 5 0 5 0 3 8 10 12 5 0 5 0 4 7 8 11 6 5 0 5 0 2 12 13",
    "42 5 0 5 0 5 2 6 8 10 14 5 0 5 0 5 3 7 9 11 15 5 0 5 0 4 0 8 10 16 5 0 5 0 4 1 9 11 16 5 0 5 0 1 12 5 0 5 0 1 13 5 0 5 0 3 0 8 12 5 0 5 0 3 1 9 13 4 5 5 0 4 0 2 6 12 5 0 4 5 4 1 3 7 13 5 0 5 0 3 0 2 14 5 0 5 0 3 1 3 15 5 0 5 0 3 4 6 8 5 0 5 0 3 5 7 9 5 0 5 0 3 0 10 15 5 0 5 0 3 1 11 14 5 0 5 0 3 2 3 28 5 0 5 0 7 19 20 22 31 34 37 41 5 0 5 0 6 23 24 26 27 35 38 5 0 5 0 3 17 20 27 5 0 5 0 5 17 19 27 28 34 5 0 5 0 3 31 39 41 5 0 5 0 4 17 31 34 39 5 0 5 0 4 18 24 30 40 5 0 5 0 3 18 23 38 5 0 5 0 4 30 33 35 40 5 0 5 0 3 18 35 36 5 0 5 0 5 18 19 20 29 32 5 0 5 0 4 20 29 34 16 5 0 5 0 3 27 28 38 5 0 5 0 3 23 25 33 5 0 5 0 4 17 21 22 39 5 0 5 0 3 27 36 37 5 0 5 0 2 25 30 5 0 5 0 4 17 20 22 28 5 0 5 0 4 18 25 26 40 5 0 5 0 2 26 32 5 0 5 0 3 17 32 41 5 0 5 0 3 18 24 29 5 0 5 0 3 21 22 31 5 0 5 0 3 23 25 35 5 0 5 0 3 17 21 37",
    "32 5 0 5 0 4 1 2 4 6 5 0 5 0 4 0 3 5 7 5 0 5 0 3 0 4 16 5 0 5 0 3 1 5 16 5 0 4 5 4 0 2 6 12 4 5 5 0 4 1 3 7 13 5 0 5 0 4 0 4 8 12 5 0 5 0 4 1 5 9 13 5 0 5 0 3 6 10 14 5 0 5 0 3 7 11 15 5 0 5 0 3 8 11 14 5 0 5 0 3 9 10 15 5 0 5 0 2 4 6 5 0 5 0 2 5 7 5 0 5 0 2 8 10 5 0 5 0 2 9 11 5 0 5 0 3 2 3 29 5 0 5 0 5 18 22 25 27 28 5 0 5 0 3 17 25 26 5 0 5 0 4 20 24 29 30 5 0 5 0 3 19 23 24 5 0 5 0 2 25 26 5 0 5 0 3 17 28 31 5 0 5 0 2 20 24 5 0 5 0 4 19 20 23 30 5 0 5 0 5 17 18 21 26 31 5 0 5 0 3 18 21 25 5 0 5 0 3 17 28 30 5 0 5 0 3 17 22 27 5 0 5 0 3 19 30 16 5 0 5 0 4 19 24 27 29 5 0 5 0 2 22 25",
    "26 5 0 5 0 3 2 8 12 5 0 5 0 3 3 9 13 5 0 5 0 3 0 6 8 5 0 5 0 3 1 7 9 5 0 5 0 3 6 8 10 5 0 5 0 3 7 9 11 5 0 5 0 3 2 4 8 5 0 5 0 3 3 5 9 5 0 5 0 5 0 2 4 6 12 5 0 5 0 5 1 3 5 7 13 4 5 5 0 2 4 12 5 0 4 5 2 5 13 5 0 5 0 4 0 8 10 14 5 0 5 0 4 1 9 11 14 5 0 5 0 3 13 12 18 5 0 5 0 4 16 19 21 25 5 0 5 0 5 15 19 22 23 25 5 0 5 0 4 18 20 23 24 5 0 5 0 4 17 23 25 14 5 0 5 0 4 15 16 21 22 5 0 5 0 1 17 5 0 5 0 2 15 19 5 0 5 0 4 16 19 23 24 5 0 5 0 5 16 17 18 22 24 5 0 5 0 3 17 22 23 5 0 5 0 3 15 16 18",
    "23 5 0 5 0 3 4 6 8 5 0 5 0 3 5 7 9 5 0 5 0 1 8 5 0 5 0 1 9 5 0 5 0 5 0 6 8 5 10 5 0 5 0 5 1 7 9 4 10 5 0 5 0 2 0 4 5 0 5 0 2 1 5 5 0 4 5 3 0 2 4 4 5 5 0 3 1 3 5 5 0 5 0 3 4 5 15 5 0 5 0 4 16 19 20 22 5 0 5 0 4 13 14 15 18 5 0 5 0 4 12 18 20 22 5 0 5 0 3 12 15 18 5 0 5 0 3 12 14 10 5 0 5 0 4 11 17 19 21 5 0 5 0 3 16 19 21 5 0 5 0 4 12 13 14 22 5 0 5 0 4 11 16 17 22 5 0 5 0 3 11 13 22 5 0 5 0 2 16 17 5 0 5 0 5 11 13 18 19 20",
    "38 5 0 5 0 7 5 9 13 16 23 28 30 5 0 5 0 7 4 8 12 17 22 29 31 5 0 5 0 6 6 8 10 24 34 35 5 0 5 0 6 7 9 11 25 35 34 5 0 5 0 7 1 7 12 14 22 26 37 5 0 5 0 7 0 6 13 15 23 27 36 5 0 5 0 5 2 5 23 35 36 5 0 5 0 5 3 4 22 34 37 5 0 5 0 5 1 2 10 17 22 5 0 5 0 5 0 3 11 16 23 5 0 5 0 3 2 8 17 5 0 5 0 3 3 9 16 5 0 5 0 4 1 4 14 29 5 0 5 0 4 0 5 15 28 5 0 5 0 5 4 12 18 26 29 5 0 5 0 5 5 13 19 27 28 5 0 5 0 4 0 9 11 30 5 0 5 0 4 1 8 10 31 5 0 5 0 3 14 26 32 5 0 5 0 3 15 27 33 5 0 5 0 3 24 33 36 5 0 5 0 3 25 32 37 5 0 5 0 5 1 4 7 8 34 5 0 5 0 5 0 5 6 9 35 5 0 5 0 3 2 20 36 5 0 5 0 3 3 21 37 5 0 5 0 3 4 14 18 5 0 5 0 3 5 15 19 5 0 5 0 3 0 13 15 5 0 5 0 3 1 12 14 5 0 5 0 2 0 16 5 0 5 0 2 1 17 4 5 5 0 2 18 21 5 0 4 5 2 19 20 5 0 5 0 5 2 3 7 22 35 5 0 5 0 5 3 2 6 23 34 5 0 5 0 4 5 6 20 24 5 0 5 0 4 4 7 21 25",
    "38 5 0 5 0 5 2 11 14 28 30 5 0 5 0 5 3 10 15 29 31 5 0 5 0 5 0 11 13 16 30 5 0 5 0 5 1 10 12 17 31 5 0 4 5 7 7 8 13 16 25 26 37 4 5 5 0 7 6 9 12 17 24 27 36 5 0 5 0 3 5 23 36 5 0 5 0 3 4 22 37 5 0 5 0 4 4 18 20 25 5 0 5 0 4 5 19 21 24 5 0 5 0 7 1 3 12 18 20 32 35 5 0 5 0 7 0 2 13 19 21 33 34 5 0 5 0 7 3 5 10 17 21 24 35 5 0 5 0 7 2 4 11 16 20 25 34 5 0 5 0 3 0 28 33 5 0 5 0 3 1 29 32 5 0 5 0 5 2 4 13 26 30 5 0 5 0 5 3 5 12 27 31 5 0 5 0 5 8 10 20 32 22 5 0 5 0 5 9 11 21 33 23 5 0 5 0 4 8 10 13 18 5 0 5 0 4 9 11 12 19 5 0 5 0 3 7 18 32 5 0 5 0 3 6 19 33 5 0 5 0 3 5 9 12 5 0 5 0 3 4 8 13 5 0 5 0 3 4 16 30 5 0 5 0 3 5 17 31 5 0 5 0 3 0 14 30 5 0 5 0 3 1 15 31 5 0 5 0 5 0 2 16 26 28 5 0 5 0 5 1 3 17 27 29 5 0 5 0 4 10 15 18 22 5 0 5 0 4 11 14 19 23 5 0 5 0 3 11 13 35 5 0 5 0 3 10 12 34 5 0 5 0 2 5 6 5 0 5 0 2 4 7",
    "38 5 0 5 0 3 6 10 20 5 0 5 0 3 7 11 21 5 0 5 0 5 4 12 20 22 34 5 0 5 0 5 5 13 21 23 35 5 0 5 0 5 2 16 22 34 36 5 0 5 0 5 3 17 23 35 37 4 5 5 0 3 0 16 30 5 0 4 5 3 1 17 31 5 0 5 0 4 10 12 14 20 5 0 5 0 4 11 13 15 21 5 0 5 0 5 0 8 20 24 28 5 0 5 0 5 1 9 21 25 29 5 0 5 0 5 2 8 13 14 20 5 0 5 0 5 3 9 12 15 21 5 0 5 0 3 8 12 15 5 0 5 0 3 9 13 14 5 0 5 0 6 4 6 18 22 26 30 5 0 5 0 6 5 7 19 23 27 31 5 0 5 0 3 16 26 30 5 0 5 0 3 17 27 31 5 0 5 0 6 0 2 8 10 12 34 5 0 5 0 6 1 3 9 11 13 35 5 0 5 0 3 2 4 16 5 0 5 0 3 3 5 17 5 0 5 0 2 10 28 5 0 5 0 2 11 29 5 0 5 0 3 16 18 27 5 0 5 0 3 17 19 26 5 0 5 0 3 10 24 32 5 0 5 0 3 11 25 33 5 0 5 0 3 6 16 18 5 0 5 0 3 7 17 19 5 0 5 0 1 28 5 0 5 0 1 29 5 0 5 0 4 2 4 20 36 5 0 5 0 4 3 5 21 37 5 0 5 0 2 4 34 5 0 5 0 2 5 35",
    "84 4 5 5 0 2 1 2 5 0 5 0 4 0 2 3 4 5 0 5 0 4 0 1 4 5 5 0 5 0 3 1 4 6 5 0 5 0 6 1 2 3 5 6 7 5 0 5 0 3 2 4 7 5 0 5 0 4 3 4 7 8 5 0 5 0 4 4 5 6 9 5 0 5 0 2 6 13 5 0 5 0 2 7 14 5 0 5 0 2 11 16 5 0 5 0 3 10 16 17 5 0 5 0 3 13 18 19 5 0 5 0 4 8 12 19 20 5 0 5 0 4 9 15 21 22 5 0 5 0 3 14 22 23 5 0 5 0 5 10 11 17 24 25 5 0 5 0 4 11 16 18 25 5 0 5 0 4 12 17 19 26 5 0 5 0 6 12 13 18 20 26 27 5 0 5 0 3 13 19 27 5 0 5 0 3 14 22 28 5 0 5 0 6 14 15 21 23 28 29 5 0 5 0 4 15 22 29 32 5 0 5 0 2 16 25 5 0 5 0 3 16 17 24 5 0 5 0 3 18 19 27 5 0 5 0 4 19 20 26 30 5 0 5 0 4 21 22 29 31 5 0 5 0 3 22 23 28 5 0 5 0 2 27 33 5 0 5 0 2 28 34 5 0 5 0 2 23 35 5 0 5 0 4 30 34 38 39 5 0 5 0 4 31 33 39 40 5 0 5 0 4 32 36 41 42 5 0 5 0 3 35 42 43 5 0 5 0 2 44 45 5 0 5 0 3 33 39 46 5 0 5 0 6 33 34 38 40 46 47 5 0 5 0 3 34 39 47 5 0 5 0 3 35 42 48 5 0 5 0 6 35 36 41 43 48 49 5 0 5 0 4 36 42 44 49 5 0 5 0 4 37 43 45 50 5 0 5 0 3 37 44 50 5 0 5 0 4 38 39 47 51 5 0 5 0 4 39 40 46 51 5 0 5 0 4 41 42 49 52 5 0 5 0 4 42 43 48 53 5 0 5 0 3 44 45 54 5 0 4 5 2 46 47 5 0 5 0 2 48 55 5 0 5 0 2 49 56 5 0 5 0 2 50 57 5 0 5 0 3 52 60 61 5 0 5 0 4 53 57 63 64 5 0 5 0 4 54 56 64 65 5 0 5 0 3 59 66 67 5 0 5 0 4 58 60 67 68 5 0 5 0 4 55 59 61 68 5 0 5 0 3 55 60 62 5 0 5 0 2 61 63 5 0 5 0 4 56 62 64 69 5 0 5 0 6 56 57 63 65 69 70 5 0 5 0 3 57 64 70 5 0 5 0 3 58 67 71 5 0 5 0 6 58 59 66 68 71 72 5 0 5 0 4 59 60 67 72 5 0 5 0 4 63 64 70 73 5 0 5 0 3 64 65 69 5 0 5 0 4 66 67 72 74 5 0 5 0 5 67 68 71 74 75 5 0 5 0 2 69 76 5 0 5 0 5 71 72 75 77 78 5 0 5 0 4 72 74 78 79 5 0 5 0 2 73 81 5 0 5 0 3 74 78 82 5 0 5 0 6 74 75 77 79 82 83 5 0 5 0 4 75 78 80 83 5 0 5 0 2 79 81 5 0 5 0 2 76 80 5 0 5 0 3 77 78 83 5 0 5 0 3 78 79 82",
    "22 5 0 5 0 3 2 4 6 5 0 5 0 3 3 5 7 5 0 5 0 3 0 4 3 5 0 5 0 3 1 5 2 5 0 5 0 2 0 2 5 0 5 0 2 1 3 4 5 5 0 2 0 8 5 0 4 5 2 1 8 5 0 5 0 3 7 6 21 5 0 5 0 3 13 15 19 5 0 5 0 4 14 16 17 18 5 0 5 0 4 12 16 17 20 5 0 5 0 3 11 17 20 5 0 5 0 3 9 15 18 5 0 5 0 4 10 16 18 21 5 0 5 0 5 9 13 18 19 21 5 0 5 0 4 10 11 14 17 5 0 5 0 4 10 11 12 16 5 0 5 0 5 10 13 14 15 21 5 0 5 0 3 9 15 21 5 0 5 0 2 11 12 5 0 5 0 5 14 15 18 19 8",
    "32 4 5 5 0 4 8 14 24 26 5 0 4 5 4 9 15 25 27 5 0 5 0 4 4 20 28 30 5 0 5 0 4 5 21 29 31 5 0 5 0 4 2 12 28 30 5 0 5 0 4 3 13 29 31 5 0 5 0 5 8 16 18 20 26 5 0 5 0 5 9 17 19 21 27 5 0 5 0 5 0 6 18 24 26 5 0 5 0 5 1 7 19 25 27 5 0 5 0 3 12 16 18 5 0 5 0 3 13 17 19 5 0 5 0 3 4 10 28 5 0 5 0 3 5 11 29 5 0 5 0 1 0 5 0 5 0 1 1 5 0 5 0 4 6 10 18 20 5 0 5 0 4 7 11 19 21 5 0 5 0 4 6 8 10 16 5 0 5 0 4 7 9 11 17 5 0 5 0 3 2 6 16 5 0 5 0 3 3 7 17 5 0 5 0 2 23 30 5 0 5 0 2 22 31 5 0 5 0 2 0 8 5 0 5 0 2 1 9 5 0 5 0 3 0 6 8 5 0 5 0 3 1 7 9 5 0 5 0 3 2 4 12 5 0 5 0 3 3 5 13 5 0 5 0 3 2 4 22 5 0 5 0 3 3 5 23",
    "38 5 0 5 0 4 2 10 17 29 5 0 5 0 4 3 11 16 28 5 0 5 0 2 0 20 5 0 5 0 2 1 21 5 0 5 0 5 9 15 22 31 35 5 0 5 0 5 8 14 23 30 34 5 0 5 0 4 9 18 24 31 5 0 5 0 4 8 19 25 30 5 0 5 0 6 5 7 23 27 30 25 5 0 5 0 6 4 6 22 26 31 24 5 0 5 0 7 0 13 20 25 27 29 33 5 0 5 0 7 1 12 21 24 26 28 32 5 0 5 0 3 11 13 23 5 0 5 0 3 10 12 22 5 0 5 0 4 5 16 23 34 5 0 5 0 4 4 17 22 35 5 0 5 0 4 1 14 23 28 5 0 5 0 4 0 15 22 29 5 0 5 0 2 6 31 5 0 5 0 2 7 30 5 0 5 0 3 2 10 33 5 0 5 0 3 3 11 32 5 0 4 5 7 4 9 13 15 17 26 29 4 5 5 0 7 5 8 12 14 16 27 28 5 0 5 0 4 6 9 11 32 5 0 5 0 4 7 8 10 33 5 0 5 0 3 9 11 22 5 0 5 0 3 8 10 23 5 0 5 0 4 1 11 16 23 5 0 5 0 4 0 10 17 22 5 0 5 0 4 5 7 8 19 5 0 5 0 4 4 6 9 18 5 0 5 0 4 11 21 24 36 5 0 5 0 4 10 20 25 37 5 0 5 0 2 5 14 5 0 5 0 2 4 15 5 0 5 0 1 32 5 0 5 0 1 33",
    "34 5 0 5 0 3 8 11 15 5 0 5 0 3 9 10 14 4 5 5 0 3 13 16 20 5 0 4 5 3 12 17 21 5 0 5 0 7 6 9 11 18 21 26 29 5 0 5 0 7 7 8 10 19 20 27 28 5 0 5 0 4 4 9 26 31 5 0 5 0 4 5 8 27 30 5 0 5 0 3 0 5 7 5 0 5 0 3 1 4 6 5 0 5 0 5 1 5 20 24 29 5 0 5 0 5 0 4 21 25 28 5 0 5 0 4 3 17 22 32 5 0 5 0 4 2 16 23 33 5 0 5 0 4 1 16 23 31 5 0 5 0 4 0 17 22 30 5 0 5 0 3 2 13 14 5 0 5 0 3 3 12 15 5 0 5 0 3 4 21 26 5 0 5 0 3 5 20 27 5 0 5 0 5 2 5 10 19 24 5 0 5 0 5 3 4 11 18 25 5 0 5 0 3 12 15 32 5 0 5 0 3 13 14 33 5 0 5 0 2 10 20 5 0 5 0 2 11 21 5 0 5 0 4 4 6 18 31 5 0 5 0 4 5 7 19 30 5 0 5 0 2 5 11 5 0 5 0 2 4 10 5 0 5 0 3 7 15 27 5 0 5 0 3 6 14 26 5 0 5 0 2 12 22 5 0 5 0 2 13 23",
    "27 5 0 5 0 3 8 10 12 5 0 5 0 3 9 11 13 5 0 5 0 4 4 6 8 10 5 0 5 0 4 5 7 9 11 5 0 5 0 2 2 6 5 0 5 0 2 3 7 5 0 5 0 4 2 4 8 14 5 0 5 0 4 3 5 9 14 5 0 5 0 4 0 2 6 10 5 0 5 0 4 1 3 7 11 5 0 5 0 4 0 2 8 12 5 0 5 0 4 1 3 9 13 5 0 4 5 2 0 10 4 5 5 0 2 1 11 5 0 5 0 3 7 6 22 5 0 5 0 5 16 18 23 24 25 5 0 5 0 4 15 18 20 22 5 0 5 0 4 19 21 24 25 5 0 5 0 3 15 16 20 5 0 5 0 3 17 21 25 5 0 5 0 3 16 18 22 5 0 5 0 3 17 19 26 5 0 5 0 4 16 20 23 14 5 0 5 0 3 15 22 25 5 0 5 0 2 15 17 5 0 5 0 4 15 17 19 23 5 0 5 0 1 21",
    "46 5 0 5 0 5 2 6 12 14 16 5 0 5 0 5 3 7 13 15 17 5 0 5 0 3 0 6 10 5 0 5 0 3 1 7 11 5 0 5 0 4 8 14 18 20 5 0 5 0 4 9 15 19 21 5 0 5 0 5 0 2 7 10 12 5 0 5 0 5 1 3 6 11 13 5 0 5 0 2 4 20 5 0 5 0 2 5 21 5 0 5 0 4 2 6 18 22 5 0 5 0 4 3 7 19 22 5 0 5 0 3 0 6 13 5 0 5 0 3 1 7 12 4 5 5 0 3 0 4 16 5 0 4 5 3 1 5 17 5 0 5 0 3 0 14 20 5 0 5 0 3 1 15 21 5 0 5 0 2 4 10 5 0 5 0 2 5 11 5 0 5 0 3 4 8 16 5 0 5 0 3 5 9 17 5 0 5 0 3 11 10 41 5 0 5 0 4 26 29 33 40 5 0 5 0 4 25 27 28 30 5 0 5 0 3 24 28 30 5 0 5 0 3 23 32 42 5 0 5 0 6 24 28 35 36 37 45 5 0 5 0 4 24 25 27 36 5 0 5 0 5 23 31 37 39 45 5 0 5 0 4 24 25 36 44 5 0 5 0 3 29 33 41 5 0 5 0 6 26 33 34 38 42 43 5 0 5 0 5 23 31 32 41 42 5 0 5 0 3 32 38 43 5 0 5 0 2 27 45 5 0 5 0 4 27 28 30 37 5 0 5 0 5 27 29 36 39 45 5 0 5 0 3 32 34 41 5 0 5 0 3 29 37 40 5 0 5 0 2 23 39 5 0 5 0 4 31 33 38 22 5 0 5 0 3 26 32 33 5 0 5 0 2 32 34 5 0 5 0 1 30 5 0 5 0 4 27 29 35 37",
    "20 5 0 5 0 5 2 4 6 8 1 5 0 5 0 5 3 5 7 9 0 5 0 4 5 3 0 8 3 4 5 5 0 3 1 9 2 5 0 5 0 3 0 6 10 5 0 5 0 3 1 7 10 5 0 5 0 3 0 4 8 5 0 5 0 3 1 5 9 5 0 5 0 3 0 2 6 5 0 5 0 3 1 3 7 5 0 5 0 3 5 4 19 5 0 5 0 4 14 15 17 19 5 0 5 0 3 14 16 18 5 0 5 0 2 15 19 5 0 5 0 5 11 12 17 18 19 5 0 5 0 4 11 13 17 19 5 0 5 0 1 12 5 0 5 0 3 11 14 15 5 0 5 0 2 12 14 5 0 5 0 5 11 13 14 15 10",
    "27 5 0 5 0 5 2 4 6 8 10 5 0 5 0 5 3 5 7 9 10 5 0 5 0 2 0 6 5 0 5 0 2 1 7 5 0 5 0 3 0 6 8 5 0 5 0 3 1 7 9 5 0 5 0 4 0 2 4 7 5 0 5 0 4 1 3 5 6 5 0 4 5 2 0 4 4 5 5 0 2 1 5 5 0 5 0 3 0 1 14 5 0 5 0 3 13 12 14 5 0 5 0 2 11 14 5 0 5 0 4 11 15 16 14 5 0 5 0 4 11 13 12 10 5 0 5 0 3 17 16 13 5 0 5 0 4 17 15 13 18 5 0 5 0 3 19 15 16 5 0 5 0 3 20 16 21 5 0 5 0 3 22 17 21 5 0 5 0 4 22 21 18 23 5 0 5 0 4 22 20 19 18 5 0 5 0 6 20 24 25 19 21 23 5 0 5 0 3 22 20 24 5 0 5 0 3 22 23 26 5 0 5 0 2 22 26 5 0 5 0 2 24 25",
    "36 5 0 5 0 5 7 8 13 16 26 5 0 5 0 5 6 9 12 17 27 5 0 5 0 4 9 11 24 30 4 2 5 0 4 8 10 25 31 5 0 4 0 5 9 16 19 24 27 4 1 5 0 5 8 17 18 25 26 5 0 5 0 4 1 12 20 28 5 0 5 0 4 0 13 21 29 5 0 5 0 3 0 3 5 5 0 5 1 3 1 2 4 5 0 5 0 3 3 22 33 5 0 5 0 3 2 23 32 5 0 5 0 3 1 6 30 5 0 5 0 3 0 7 31 5 0 5 0 4 16 21 29 34 5 0 5 0 4 17 20 28 35 5 0 5 1 6 0 4 14 26 27 29 5 0 5 0 6 1 5 15 27 26 28 4 1 5 0 4 5 22 33 25 5 0 5 1 4 4 23 32 24 5 0 5 0 2 6 15 5 0 5 0 2 7 14 5 0 5 0 3 10 18 33 5 0 5 0 3 11 19 32 5 0 4 6 4 2 4 19 32 4 5 5 0 4 3 5 18 33 5 0 5 0 4 0 5 16 17 5 0 5 1 4 1 4 17 16 5 0 5 0 3 6 15 17 5 0 5 0 3 7 14 16 5 0 5 0 2 2 12 5 0 5 0 2 3 13 5 0 5 0 4 11 19 23 24 4 1 5 0 4 10 18 22 25 5 0 5 0 1 14 5 0 5 0 1 15",
    "40 4 5 5 0 5 4 1 2 5 3 5 0 5 0 4 0 4 6 5 5 0 5 0 3 0 7 8 5 0 5 0 2 0 9 5 0 5 0 7 0 10 8 12 1 6 11 5 0 5 0 3 0 9 1 5 0 5 0 3 4 10 1 5 0 5 0 4 13 15 2 14 5 0 5 0 5 4 17 16 2 14 5 0 5 0 3 13 3 5 5 0 5 0 3 4 6 11 5 0 5 0 4 4 10 12 18 5 0 5 0 4 4 17 11 18 5 0 5 0 4 15 7 9 19 5 0 5 0 6 22 16 7 8 21 20 5 0 5 0 6 13 22 7 24 23 25 5 0 5 0 5 17 8 21 14 26 5 0 5 0 6 27 16 28 8 12 18 5 0 5 0 4 17 12 11 29 5 0 5 0 2 13 25 5 0 5 0 2 22 14 5 0 5 0 6 16 22 28 24 14 26 5 0 5 0 5 15 24 14 21 20 5 0 5 0 4 30 15 31 25 5 0 5 0 5 30 15 22 32 21 5 0 5 0 4 15 23 31 19 5 0 5 0 2 16 21 5 0 5 0 4 17 28 33 29 5 0 5 0 4 27 17 32 21 5 0 5 0 2 27 18 5 0 5 0 7 35 36 24 23 37 34 31 5 0 5 0 4 30 36 23 25 5 0 5 0 3 35 28 24 5 0 5 0 3 27 38 39 5 0 5 0 3 30 36 37 5 0 4 5 5 30 37 32 39 38 5 0 5 0 3 30 34 31 5 0 5 0 4 35 30 34 39 5 0 5 0 2 35 33 5 0 5 0 3 35 33 37",
    "32 4 5 5 0 5 9 14 18 21 24 5 0 4 5 5 8 15 19 20 25 5 0 5 0 6 3 12 13 14 17 18 5 0 5 0 6 2 13 12 15 16 19 5 0 5 0 3 10 12 27 5 0 5 0 3 11 13 26 5 0 5 0 3 12 17 23 5 0 5 0 3 13 16 22 5 0 5 0 5 1 10 15 30 20 5 0 5 0 5 0 11 14 31 21 5 0 5 0 3 4 8 12 5 0 5 0 3 5 9 13 5 0 5 0 7 2 3 4 6 10 17 30 5 0 5 0 7 3 2 5 7 11 16 31 5 0 5 0 5 0 2 9 18 31 5 0 5 0 5 1 3 8 19 30 5 0 5 0 7 3 7 13 19 22 25 29 5 0 5 0 7 2 6 12 18 23 24 28 5 0 5 0 5 0 2 14 17 24 5 0 5 0 5 1 3 15 16 25 5 0 5 0 3 1 8 25 5 0 5 0 3 0 9 24 5 0 5 0 4 7 16 26 29 5 0 5 0 4 6 17 27 28 5 0 5 0 4 0 17 18 21 5 0 5 0 4 1 16 19 20 5 0 5 0 2 5 22 5 0 5 0 2 4 23 5 0 5 0 2 17 23 5 0 5 0 2 16 22 5 0 5 0 3 8 12 15 5 0 5 0 3 9 13 14",
    "57 5 0 5 0 3 1 8 12 5 0 5 0 3 0 9 13 5 0 5 0 5 4 8 14 18 20 5 0 5 0 5 5 9 15 19 21 5 0 5 0 6 2 6 8 12 18 22 5 0 5 0 6 3 7 9 13 19 23 5 0 5 0 3 4 10 12 5 0 5 0 3 5 11 13 5 0 4 5 4 0 2 4 12 4 5 5 0 4 1 3 5 13 5 0 5 0 4 6 16 22 24 5 0 5 0 4 7 17 23 24 5 0 5 0 4 0 4 6 8 5 0 5 0 4 1 5 7 9 5 0 5 0 3 2 15 20 5 0 5 0 3 3 14 21 5 0 5 0 2 10 22 5 0 5 0 2 11 23 5 0 5 0 3 2 4 20 5 0 5 0 3 3 5 21 5 0 5 0 3 2 14 18 5 0 5 0 3 3 15 19 5 0 5 0 3 4 10 16 5 0 5 0 3 5 11 17 5 0 5 0 3 10 11 26 5 0 5 0 6 28 31 34 35 38 42 5 0 5 0 4 35 38 47 24 5 0 5 0 5 32 40 45 46 50 5 0 5 0 4 25 30 34 49 5 0 5 0 3 32 40 46 5 0 5 0 5 28 42 44 49 54 5 0 5 0 5 25 38 39 42 43 5 0 5 0 5 27 29 46 51 53 5 0 5 0 3 34 48 55 5 0 5 0 3 25 28 33 5 0 5 0 4 25 26 38 56 5 0 5 0 6 41 43 47 51 52 53 5 0 5 0 2 40 50 5 0 5 0 5 25 26 31 35 43 5 0 5 0 4 31 41 42 43 5 0 5 0 4 27 29 37 50 5 0 5 0 4 36 39 43 52 5 0 5 0 5 25 30 31 39 54 5 0 5 0 6 31 36 38 39 41 47 5 0 5 0 3 30 49 55 5 0 5 0 1 27 5 0 5 0 3 27 29 32 5 0 5 0 4 26 36 43 53 5 0 5 0 2 33 56 5 0 5 0 3 28 30 44 5 0 5 0 3 27 37 40 5 0 5 0 4 32 36 52 53 5 0 5 0 3 36 41 51 5 0 5 0 4 32 36 47 51 5 0 5 0 2 30 42 5 0 5 0 2 33 44 5 0 5 0 2 35 48",
    "34 5 0 5 0 5 11 12 24 26 28 5 0 5 0 5 10 13 25 27 29 5 0 5 0 4 8 11 16 19 5 0 5 0 4 9 10 17 18 5 0 5 0 3 5 6 18 5 0 5 0 3 4 7 19 5 0 5 0 6 4 19 20 27 29 30 5 0 5 0 6 5 18 21 26 28 31 5 0 5 0 5 2 16 19 20 33 5 0 5 0 5 3 17 18 21 32 5 0 5 0 5 1 3 17 23 25 5 0 5 0 5 0 2 16 22 24 5 0 5 0 3 0 28 31 5 0 5 0 3 1 29 30 5 0 5 0 3 17 25 32 5 0 5 0 3 16 24 33 5 0 5 0 5 2 8 11 15 24 5 0 5 0 5 3 9 10 14 25 5 0 5 0 6 3 4 7 9 23 21 5 0 5 0 6 2 5 6 8 22 20 5 0 5 0 5 6 8 19 30 33 5 0 5 0 5 7 9 18 31 32 5 0 5 0 3 11 19 26 5 0 5 0 3 10 18 27 4 5 5 0 4 0 11 15 16 5 0 4 5 4 1 10 14 17 5 0 5 0 3 0 7 22 5 0 5 0 3 1 6 23 5 0 5 0 4 0 7 12 31 5 0 5 0 4 1 6 13 30 5 0 5 0 4 6 13 20 29 5 0 5 0 4 7 12 21 28 5 0 5 0 3 9 14 21 5 0 5 0 3 8 15 20",
    "36 5 0 5 0 5 3 4 2 1 5 5 0 5 0 4 0 7 6 5 5 0 5 0 3 0 8 4 5 0 5 0 3 0 10 9 5 0 5 0 4 0 9 8 2 5 0 5 0 2 0 1 5 0 5 0 3 7 1 11 5 0 5 0 3 6 12 1 5 0 5 0 5 9 13 4 14 2 5 0 5 0 6 13 16 3 8 4 15 5 0 5 0 6 17 3 16 19 18 12 5 0 5 0 4 17 6 20 21 5 0 5 0 4 10 7 20 22 5 0 5 0 7 9 26 23 25 24 8 14 5 0 5 0 3 13 24 8 5 0 5 0 4 9 28 26 27 5 0 5 0 3 29 9 10 5 0 5 0 7 10 20 21 11 31 19 30 5 0 5 0 4 29 10 19 32 5 0 5 0 5 10 17 18 30 32 5 0 5 0 3 17 12 11 4 5 5 0 3 17 31 11 5 0 5 0 1 12 5 0 4 5 3 13 24 25 5 0 5 0 3 13 14 23 5 0 5 0 4 13 33 26 23 5 0 5 0 3 13 15 25 5 0 5 0 1 15 5 0 5 0 3 33 15 34 5 0 5 0 5 16 18 32 34 35 5 0 5 0 3 17 31 19 5 0 5 0 3 17 30 21 5 0 5 0 3 29 19 18 5 0 5 0 3 28 34 25 5 0 5 0 4 29 28 33 35 5 0 5 0 2 29 34",
    "41 5 0 5 0 5 2 4 6 12 14 5 0 5 0 5 3 5 7 13 15 5 0 5 0 3 0 12 14 5 0 5 0 3 1 13 15 4 5 5 0 4 0 6 8 10 5 0 4 5 4 1 7 9 11 5 0 5 0 4 0 4 10 14 5 0 5 0 4 1 5 11 15 5 0 5 0 2 4 10 5 0 5 0 2 5 11 5 0 5 0 3 4 6 8 5 0 5 0 3 5 7 9 5 0 5 0 3 0 2 13 5 0 5 0 3 1 3 12 5 0 5 0 4 0 2 6 16 5 0 5 0 4 1 3 7 16 5 0 5 0 3 15 14 38 5 0 5 0 5 22 25 30 31 32 5 0 5 0 4 25 33 39 40 5 0 5 0 5 21 24 27 28 35 5 0 5 0 5 22 26 31 32 37 5 0 5 0 6 19 23 26 27 28 37 5 0 5 0 4 17 20 31 32 5 0 5 0 3 21 26 28 5 0 5 0 5 19 28 34 35 38 5 0 5 0 6 17 18 29 30 33 40 5 0 5 0 3 20 21 23 5 0 5 0 3 19 21 35 5 0 5 0 5 19 21 23 24 36 5 0 5 0 2 25 30 5 0 5 0 3 17 25 29 5 0 5 0 5 17 20 22 33 39 5 0 5 0 3 17 20 22 5 0 5 0 4 18 25 31 39 5 0 5 0 1 24 5 0 5 0 3 19 24 27 5 0 5 0 2 28 38 5 0 5 0 2 20 21 5 0 5 0 3 24 36 16 5 0 5 0 3 18 31 33 5 0 5 0 2 18 25",
    "40 5 0 5 0 4 4 6 9 10 5 0 5 0 4 5 7 8 11 5 0 5 0 6 11 19 23 34 37 38 5 0 5 0 6 10 18 22 35 36 39 5 0 5 0 7 0 9 13 16 24 29 30 5 0 5 0 7 1 8 12 17 25 28 31 5 0 5 0 3 0 24 33 5 0 5 0 3 1 25 32 5 0 5 0 6 1 5 11 23 30 31 5 0 5 0 6 0 4 10 22 31 30 4 5 5 0 3 0 3 9 5 0 4 5 3 1 2 8 5 0 5 0 7 5 14 17 22 27 31 35 5 0 5 0 7 4 15 16 23 26 30 34 5 0 5 0 4 12 17 27 28 5 0 5 0 4 13 16 26 29 5 0 5 0 4 4 13 15 29 5 0 5 0 4 5 12 14 28 5 0 5 0 4 3 21 36 39 5 0 5 0 4 2 20 37 38 5 0 5 0 3 19 32 37 5 0 5 0 3 18 33 36 5 0 5 0 4 3 9 12 35 5 0 5 0 4 2 8 13 34 5 0 5 0 2 4 6 5 0 5 0 2 5 7 5 0 5 0 3 13 15 38 5 0 5 0 3 12 14 39 5 0 5 0 3 5 14 17 5 0 5 0 3 4 15 16 5 0 5 0 4 4 8 9 13 5 0 5 0 4 5 9 8 12 5 0 5 0 2 7 20 5 0 5 0 2 6 21 5 0 5 0 3 2 13 23 5 0 5 0 3 3 12 22 5 0 5 0 3 3 18 21 5 0 5 0 3 2 19 20 5 0 5 0 3 2 19 26 5 0 5 0 3 3 18 27",
    "18 5 0 5 0 2 2 6 5 0 5 0 2 3 7 5 0 5 0 3 0 4 6 5 0 5 0 3 1 5 7 5 0 5 0 3 2 5 6 5 0 5 0 3 3 4 7 4 5 5 0 4 0 2 4 8 5 0 4 5 4 1 3 5 8 5 0 5 0 3 7 6 10 5 0 5 0 5 10 11 12 13 14 5 0 5 0 4 9 12 13 8 5 0 5 0 3 9 13 16 5 0 5 0 4 9 10 14 17 5 0 5 0 4 9 10 11 16 5 0 5 0 3 9 12 15 5 0 5 0 2 14 17 5 0 5 0 2 11 13 5 0 5 0 2 12 15",
    "84 4 5 5 0 2 1 2 5 0 5 0 4 0 2 3 4 5 0 5 0 4 0 1 4 5 5 0 5 0 3 1 4 6 5 0 5 0 6 1 2 3 5 6 7 5 0 5 0 3 2 4 7 5 0 5 0 4 3 4 7 8 5 0 5 0 4 4 5 6 9 5 0 5 0 2 6 13 5 0 5 0 2 7 14 5 0 5 0 2 11 16 5 0 5 0 3 10 16 17 5 0 5 0 3 13 18 19 5 0 5 0 4 8 12 19 20 5 0 5 0 4 9 15 21 22 5 0 5 0 3 14 22 23 5 0 5 0 5 10 11 17 24 25 5 0 5 0 4 11 16 18 25 5 0 5 0 4 12 17 19 26 5 0 5 0 6 12 13 18 20 26 27 5 0 5 0 3 13 19 27 5 0 5 0 3 14 22 28 5 0 5 0 6 14 15 21 23 28 29 5 0 5 0 4 15 22 29 32 5 0 5 0 2 16 25 5 0 5 0 3 16 17 24 5 0 5 0 3 18 19 27 5 0 5 0 4 19 20 26 30 5 0 5 0 4 21 22 29 31 5 0 5 0 3 22 23 28 5 0 5 0 2 27 33 5 0 5 0 2 28 34 5 0 5 0 2 23 35 5 0 5 0 4 30 34 38 39 5 0 5 0 4 31 33 39 40 5 0 5 0 4 32 36 41 42 5 0 5 0 3 35 42 43 5 0 5 0 2 44 45 5 0 5 0 3 33 39 46 5 0 5 0 6 33 34 38 40 46 47 5 0 5 0 3 34 39 47 5 0 5 0 3 35 42 48 5 0 5 0 6 35 36 41 43 48 49 5 0 5 0 4 36 42 44 49 5 0 5 0 4 37 43 45 50 5 0 5 0 3 37 44 50 5 0 5 0 4 38 39 47 51 5 0 5 0 4 39 40 46 51 5 0 5 0 4 41 42 49 52 5 0 5 0 4 42 43 48 53 5 0 5 0 3 44 45 54 5 0 4 5 2 46 47 5 0 5 0 2 48 55 5 0 5 0 2 49 56 5 0 5 0 2 50 57 5 0 5 0 3 52 60 61 5 0 5 0 4 53 57 63 64 5 0 5 0 4 54 56 64 65 5 0 5 0 3 59 66 67 5 0 5 0 4 58 60 67 68 5 0 5 0 4 55 59 61 68 5 0 5 0 3 55 60 62 5 0 5 0 2 61 63 5 0 5 0 4 56 62 64 69 5 0 5 0 6 56 57 63 65 69 70 5 0 5 0 3 57 64 70 5 0 5 0 3 58 67 71 5 0 5 0 6 58 59 66 68 71 72 5 0 5 0 4 59 60 67 72 5 0 5 0 4 63 64 70 73 5 0 5 0 3 64 65 69 5 0 5 0 4 66 67 72 74 5 0 5 0 5 67 68 71 74 75 5 0 5 0 2 69 76 5 0 5 0 5 71 72 75 77 78 5 0 5 0 4 72 74 78 79 5 0 5 0 2 73 81 5 0 5 0 3 74 78 82 5 0 5 0 6 74 75 77 79 82 83 5 0 5 0 4 75 78 80 83 5 0 5 0 2 79 81 5 0 5 0 2 76 80 5 0 5 0 3 77 78 83 5 0 5 0 3 78 79 82"
    };
    for(const string &m:Map_Pool){
        if(count(Map_Pool.begin(),Map_Pool.end(),m)>1){
            cerr << "Warning: Duplicate map: " << endl;
            cerr << m << endl;
        }
    }
    state S;
    uniform_int_distribution<int> Map_Distrib(0,Map_Pool.size()-1);
    S.load_string(Map_Pool[Map_Distrib(generator)]);
    //S.load_string("81 4 5 5 0 2 1 2 5 0 5 0 4 0 2 3 4 5 0 5 0 4 0 1 4 5 5 0 5 0 4 1 4 6 7 5 0 5 0 6 1 2 3 5 7 8 5 0 5 0 4 2 4 8 9 5 0 5 0 4 3 7 10 11 5 0 5 0 6 3 4 6 8 11 12 5 0 5 0 6 4 5 7 9 12 13 5 0 5 0 4 5 8 13 14 5 0 5 0 4 6 11 15 16 5 0 5 0 6 6 7 10 12 16 17 5 0 5 0 6 7 8 11 13 17 18 5 0 5 0 6 8 9 12 14 18 19 5 0 5 0 4 9 13 19 20 5 0 5 0 4 10 16 21 22 5 0 5 0 5 10 11 15 17 22 5 0 5 0 5 11 12 16 18 23 5 0 5 0 6 12 13 17 19 23 24 5 0 5 0 6 13 14 18 20 24 25 5 0 5 0 4 14 19 25 26 5 0 5 0 4 15 22 28 29 5 0 5 0 4 15 16 21 29 5 0 5 0 4 17 18 24 30 5 0 5 0 6 18 19 23 25 30 31 5 0 5 0 6 19 20 24 26 31 32 5 0 5 0 4 20 25 32 33 5 0 5 0 2 34 35 5 0 5 0 4 21 29 36 37 5 0 5 0 4 21 22 28 37 5 0 5 0 4 23 24 31 38 5 0 5 0 6 24 25 30 32 38 39 5 0 5 0 6 25 26 31 33 39 40 5 0 5 0 4 26 32 40 41 5 0 5 0 4 27 35 42 43 5 0 5 0 4 27 34 43 44 5 0 5 0 3 28 37 45 5 0 5 0 5 28 29 36 45 46 5 0 5 0 5 30 31 39 47 48 5 0 5 0 6 31 32 38 40 48 49 5 0 5 0 6 32 33 39 41 49 50 5 0 5 0 4 33 40 42 50 5 0 5 0 4 34 41 43 51 5 0 5 0 6 34 35 42 44 51 52 5 0 5 0 3 35 43 52 5 0 5 0 4 36 37 46 53 5 0 5 0 4 37 45 53 54 5 0 5 0 4 38 48 55 56 5 0 5 0 6 38 39 47 49 56 57 5 0 5 0 6 39 40 48 50 57 58 5 0 5 0 4 40 41 49 58 5 0 5 0 4 42 43 52 59 5 0 5 0 4 43 44 51 59 5 0 5 0 4 45 46 54 60 5 0 5 0 4 46 53 60 61 5 0 5 0 4 47 56 62 63 5 0 5 0 6 47 48 55 57 63 64 5 0 5 0 6 48 49 56 58 64 65 5 0 5 0 4 49 50 57 65 5 0 5 0 2 51 52 5 0 5 0 4 53 54 61 66 5 0 5 0 5 54 60 62 66 67 5 0 5 0 5 55 61 63 67 68 5 0 5 0 6 55 56 62 64 68 69 5 0 5 0 6 56 57 63 65 69 70 5 0 5 0 4 57 58 64 70 5 0 5 0 4 60 61 67 71 5 0 5 0 6 61 62 66 68 71 72 5 0 5 0 6 62 63 67 69 72 73 5 0 5 0 6 63 64 68 70 73 74 5 0 5 0 4 64 65 69 74 5 0 5 0 4 66 67 72 75 5 0 5 0 6 67 68 71 73 75 76 5 0 5 0 6 68 69 72 74 76 77 5 0 5 0 4 69 70 73 77 5 0 5 0 4 71 72 76 78 5 0 5 0 6 72 73 75 77 78 79 5 0 5 0 4 73 74 76 79 5 0 5 0 4 75 76 79 80 5 0 5 0 4 76 77 78 80 5 0 4 5 2 78 79");
    const int winner{Play_Game(Bot_Names,S)};
    if(player_swap){
        return winner==-1?-1:winner==0?1:0;
    }
    else{
        return winner;
    }
}

void StopArena(const int signum){
    stop=true;
}

int main(int argc,char **argv){
    if(argc<3){
        cerr << "Program takes 2 inputs, the names of the AIs fighting each other" << endl;
        return 0;
    }
    int N_Threads{1};
    if(argc>=4){//Optional N_Threads parameter
        N_Threads=min(2*omp_get_num_procs(),max(1,atoi(argv[3])));
        cerr << "Running " << N_Threads << " arena threads" << endl;
    }
    array<string,N> Bot_Names;
    for(int i=0;i<2;++i){
        Bot_Names[i]=argv[i+1];
    }
    cout << "Testing AI " << Bot_Names[0];
    for(int i=1;i<N;++i){
        cerr << " vs " << Bot_Names[i];
    }
    cerr << endl;
    for(int i=0;i<N;++i){//Check that AI binaries are present
        ifstream Test{Bot_Names[i].c_str()};
        if(!Test){
            cerr << Bot_Names[i] << " couldn't be found" << endl;
            return 0;
        }
        Test.close();
    }
    signal(SIGTERM,StopArena);//Register SIGTERM signal handler so the arena can cleanup when you kill it
    signal(SIGPIPE,SIG_IGN);//Ignore SIGPIPE to avoid the arena crashing when an AI crashes
    int games{0},draws{0};
    array<double,2> points{0,0};
    #pragma omp parallel num_threads(N_Threads) shared(games,points,Bot_Names)
    while(!stop){
        int winner{Play_Round(Bot_Names)};
        if(winner==-1){//Draw
            #pragma omp atomic
            ++draws;
            #pragma omp atomic
            points[0]+=0.5;
            #pragma omp atomic
            points[1]+=0.5;
        }
        else{//Win
            points[winner]+=1;
        }
        #pragma omp atomic
        games+=1;
        double p{static_cast<double>(points[0])/games};
        double sigma{sqrt(p*(1-p)/games)};
        double better{0.5+0.5*erf((p-0.5)/(sqrt(2)*sigma))};
        #pragma omp critical
        cout << "Wins:" << setprecision(4) << 100*p << "+-" << 100*sigma << "% Rounds:" << games << " Draws:" << draws << " " << better*100 << "% chance that " << Bot_Names[0] << " is better" << endl;
    }
}