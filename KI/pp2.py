from scamp import *

s = Session(tempo = 80)

clarinet = s.new_part("Clarinet")

melody = [
     (62, 1),     
    (74, 3),
    (74, 1),
    (72, 1),  
    (71, 1),
    (71, 1),
    (69, 1),
    (67, 1),
    (63, 2),
    (62, 0.5),
    (60, 0.5),
    (62, 2.5),
    (60, 0.25),
    (59, 0.25),
    (60, 1),
    (63, 1.5),
    (62, 0.5),
    (62, 3),
]

melody_continuation =

for pitch, duration in melody:
    clarinet.play_note(pitch, 0.7, duration)

for pitch, duration in melody_continuation:
    clarinet.play_note(pitch, 0.7, duration)