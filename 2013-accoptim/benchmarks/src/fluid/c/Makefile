
TARGET		= fluid
BUILD		= dist

SRCDIR		= src
SOURCES		= $(wildcard $(SRCDIR)/*.c)
OBJS		= $(SOURCES:%.c=%.o)

CFLAGS		= -Wall -Werror -O2 -funroll-loops -I$(SRCDIR)
LDFLAGS		= -L/usr/X11R6/lib -lgl -lglut -lglu -framework GLUT

$(TARGET) : $(OBJS)
	-mkdir -p out
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

.PHONY: clean
clean:
	-$(RM) $(OBJS)
	-$(RM) $(TARGET)

