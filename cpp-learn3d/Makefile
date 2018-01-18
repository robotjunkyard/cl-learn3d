EXENAME=cpp-learn3d
CPPC=clang++
SRCS=main.cpp Vec.cpp Render.cpp UI.cpp CanvasDef.cpp canvas8.cpp Quat.cpp Camera.cpp Mesh.cpp
OBJS=$(subst .cpp,.o,$(SRCS))
CPPFLAGS=-g -O3 -std=c++11 -I/usr/include -I/opt/include/ # -I dep/SDL2-2.0.5/include/
LDLIBS=-lSDL2

all: $(OBJS)
	g++ $(LDFLAGS) -o $(EXENAME) $(OBJS) $(LDLIBS)

main.o: main.cpp
	$(CPPC) $(CPPFLAGS) -c main.cpp

Vec.o: Vec.cpp
	$(CPPC) $(CPPFLAGS) -c Vec.cpp

Render.o: Render.cpp
	$(CPPC) $(CPPFLAGS) -c Render.cpp

UI.o: UI.cpp
	$(CPPC) $(CPPFLAGS) -c UI.cpp

CanvasDef.o: CanvasDef.cpp
	$(CPPC) $(CPPFLAGS) -c CanvasDef.cpp

canvas8.o: canvas8.cpp
	$(CPPC) $(CPPFLAGS) -c canvas8.cpp

Quat.o: Quat.cpp
	$(CPPC) $(CPPFLAGS) -c Quat.cpp

Camera.o: Camera.cpp
	$(CPPC) $(CPPFLAGS) -c Camera.cpp

Mesh.o: Mesh.cpp
	$(CPPC) $(CPPFLAGS) -c Mesh.cpp

clean:
	rm $(OBJS) $(EXENAME)





