import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math, serial

TEMP_MAX = 22.00

xsize=200
   
def data_gen():
    t = data_gen.t
    while True:
       t+=1
       line_val = ser.readline()
       temp = float(line_val[6:11])
       print(temp)

       # Change colour of line to red to alert user that max temp has been exceeded
       if temp >= TEMP_MAX:
        line_color = "red"
       else:
        line_color = "blue"

       yield t, temp, line_color

def run(data):
    # update the data
    time,temp,color = data
    if time>-1:
        xdata.append(time)
        ydata.append(temp)
        if time>xsize: # Scroll to the left.
            ax.set_xlim(time-xsize, time)
        line.set_data(xdata, ydata)
        line.set_color(color)

    return line,

def on_close_figure(event):
    sys.exit(0)

#ATTENTION: Make sure the multimeter is configured at 9600 baud, 8-bits, parity none, 1 stop bit, echo Off????
# Configure and open host serial port
ser = serial.Serial(
    port='COM3',            # Might need to change
    baudrate=115200,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_TWO,
    bytesize=serial.EIGHTBITS
)
ser.isOpen()

data_gen.t = -1    # Start index 'time'
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot([], [], lw=2)        # plot xdata and ydata
ax.set_title('Temperature Reading')
ax.set_xlabel('Time [s]') 
ax.set_ylabel('Temperature [C]')
ax.set_ylim(0, 60)
ax.set_xlim(0, xsize)
ax.grid()
xdata, ydata = [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, repeat=False)
plt.show()