include ../Makefile.in

all:simulator

OBJS := BoundedBuffer.o Display.o fpu.o InputFile.o logger.o Ram.o SDCard.o simulator.o

simulator: $(OBJS)

# pull in dependency info for *existing* .o files
-include $(OBJS:.o=.d)

%.d: %.cpp
	g++ -MM $(CPPFLAGS) $(CXXFLAGS) $< > $@

# compile and generate dependency info
%.o: %.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) $< -o $@

clean:
	rm *.o
	rm simulator
