# inharmonicity model
# http://daffy.uah.edu/piano/page4/page8/index.html

stretch_interval = ((1.5 ** 12) / (2.0 ** 7)) ** (1.0/7)
stretch_interval
1.0019377369015756

stretch_octave = stretch_interval * 2

inharmonicity_coefficient_ratio = lambda harmonic, coeff: harmonic * (1.0 + 0.5 * (harmonic ** 2 - 1) * coeff)

stretch_coeff_2 = lambda stretch_interval: (stretch_interval - 1) / 1.5
inharmonicity_coefficient_2nd_harmonic = stretch_coeff_2(stretch_interval)
0.0012918246010504102
inharmonicity_coefficient_ratio(2, stretch_coeff_2(stretch_interval)) / 2
1.0019377369015756

stretch_coeff_3 = lambda stretch_interval: (stretch_interval - 1) / 4
inharmonicity_coefficient_3rd_harmonic = stretch_coeff_3(stretch_interval)
0.0004844342253939038
inharmonicity_coefficient_ratio(3, stretch_coeff_3(stretch_interval)) / 3
1.0019377369015756

pyth_n = lambda v, o: 7 * v - 12 * o
pyth_r = lambda v, o, stretch_interval: (1.5 ** v) / (2.0 * stretch_interval) ** o
pyth   = lambda v, o, stretch_interval: (pyth_n(v, o), pyth_r(v, o, stretch_interval))

inharmonicity_coefficient_func = lambda x, a, b, c, d, e: a + b * x + c * x * x + (d / x) + (e / (x * x))

def steinway_inharmonicity_coefficient_func(frequency):
	# empirical inharmonicity model for Steinway B
	a = 5.22964e-6
	b = 1.21012e-6
	c = 8.3666e-10
	d = -0.007927
	e = 0.429601
	return inharmonicity_coefficient_func(frequency, a, b, c, d, e)

 # mapping from MIDI numbers to note names
NOTE_NAMES = ['C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B']

def midi_to_name(n):
    """
    Convert a MIDI note number to its note name (e.g., 60 -> 'C4').
    """
    octave = n // 12 - 1
    name = NOTE_NAMES[n % 12]
    return f"{name}{octave}"

class Note:
	def __init__(self, notes, midi_n, note_frequency = None, inharmonicity_coeff = 0.0):
		self.notes = notes
		self.n = midi_n
		self.f = float(note_frequency) if note_frequency is not None else None
		self.coeff  = inharmonicity_coeff
		self.octave = inharmonicity_coefficient_ratio(2, self.coeff)
		self.parent = None
		self.children = set()

	def set_frequency(self, f):
		if self.f is None:
			self.f = float(f)
		elif self.f != f:
			print("frequency mismatch on %i %r != %r" % (self.n, self.f, f))

	def get_interval(self, delta):
		return self.notes[self.n + delta]

	def get_interval_path(self, delta):
		return self.notes.get_interval_path(self, self.get_interval(delta))

	def add_child(self, child):
		self.children.add(child)

	def set_parent(self, parent):
		self.parent = parent
		parent.add_child(self)

	def rel_tune(self, delta, n, d, use_inharmonicity = False):
		parent = self.get_interval(delta)

		if parent.f is None:
			self.notes.report()
			raise IndexError("reference parent note %i has no frequency" % parent.n)

		if use_inharmonicity:
			coeff = steinway_inharmonicity_coefficient_func(parent.f)
			stretch = inharmonicity_coefficient_ratio(2, coeff) / 2
			#print("stretch %f" % stretch)
			f = float(parent.f) * n / d * stretch
			#print("f %f -> %f" % (parent.f, f))
		else:
			f = float(parent.f) * n / d


		if self.parent is None:
			self.set_parent(parent)
			self.set_frequency(f)
		else:
			self.notes.report()
			print("parent is already assigned on %i (existing parent is %i, attempting to set %i)" % (self.n, self.parent.n, parent.n))
			raise KeyError

	def seq_tune(self, delta, n, d, use_inharmonicity = False):
		note = self.get_interval(delta)
		note.rel_tune( - delta, n, d, use_inharmonicity)

		return note
			
	def get_parent(self):
		return self.parent

	def get_ancestry(self):
		# include self in ancestry
		yield self

		node = self
		while True:
			ancestor = node.get_parent()
			if not ancestor:
				break

			yield ancestor

			node = ancestor

class Notes:
	def __init__(self):
		from sys import stdout
		self.report_file = stdout
		self.sequence = []
		self.sequence_set = set()
		self.notes = [
			Note(self, n, None, inharmonicity_coefficient_2nd_harmonic)
			for n in range(150)
		]

	def __getitem__(self, n):
		if n not in self.sequence_set:
			self.sequence_set.add(n)
			# add to sequence in order
			self.sequence.append(self.notes[n])

		return self.notes[n]

	def get_interval_path(self, noteA, noteB):
		from collections import deque
		noteA_ancestry = deque(noteA.get_ancestry())
		noteB_ancestry = deque(noteB.get_ancestry())
		#print "noteA %r noteB %r" % (noteA_ancestry, noteB_ancestry)
		while True:
			if len(noteA_ancestry) > 0 and len(noteB_ancestry) > 0 and noteA_ancestry[-1] is noteB_ancestry[-1]:
				# remove oldest common ancestor
				noteA_ancestry.pop()
				noteB_ancestry.pop()
			else:
				# all common ancestors removed
				break

		#print "common removed noteA %r noteB %r" % (noteA_ancestry, noteB_ancestry)
		# traverse down the noteA_ancestry, toward noteB
		for note in noteA_ancestry:
			yield note

		# traverse up the noteB_ancestry, toward noteB
		for note in reversed(noteB_ancestry):
			yield note

	def report(self, file = None):
		if file is not None:
			self.report_file = file
		total_key = lambda d, k: d.setdefault(k, [0])[0]
		def inc_key(d, k):
			d.setdefault(k, [0])[0] = total_key(d, k) + 1

		interval_just_count = {}
		interval_consonant_count = {}
		interval_dissonant_count = {}
		interval_path_count = {}
		interval_total = {}

		import math
		for note in self.notes:
			if note.f is not None:
				octave = note.n // 12 - 1  # MIDI octave to piano octave (integer division)
				ref_f = self.notes[60].f * 2 ** (float(note.n - 60) / 12) 
				diff_f = ref_f - note.f
				diff_r = ref_f / note.f
				cents = 1200.0 * math.log(diff_r) / math.log(2)
				print("%i: %f, %f, %f, %f" % (note.n, note.f, ref_f, diff_f, cents), file=self.report_file)
				for delta, n, d, in [
					(12, 2,  1),
					(7,  3,  2),
					(5,  4,  3),
					(4,  5,  4),
					(3,  6,  5),
					(2,  9,  8),
					(1,  17, 16),
				]:
					try:
						interval_note = note.get_interval(delta)
					except IndexError:
						continue

					if note.f is not None and interval_note.f is not None:
						interval_diff_f = note.f * n / d - interval_note.f
						interval_diff_r = note.f * n / d / interval_note.f
						cents = 1200.0 * math.log(interval_diff_r) / math.log(2)
						print("\t%i/%i offset: %f, %f" % (n, d, interval_diff_f, cents), file=self.report_file)
						inc_key(interval_total, (octave, n, d))

						if interval_diff_f == 0.0:
							inc_key(interval_just_count, (octave, n, d))

						if abs(interval_diff_f) < 0.5:
							inc_key(interval_consonant_count, (octave, n, d))

						if abs(interval_diff_f) > 2.0 and abs(interval_diff_f) < 20.0:
							inc_key(interval_dissonant_count, (octave, n, d))

						#print "path between %i and %i" % (note.n, note.n + delta)
						#for ancestor in note.get_interval_path(delta):
						#	print "\t%i" % ancestor.n

						for ancestor in note.get_interval_path(delta):
							inc_key(interval_path_count, (octave, n, d))

				print

		for octave, n, d in sorted(interval_total.keys()):
			total     = total_key(interval_total,           (octave, n, d))
			just      = total_key(interval_just_count,      (octave, n, d))
			consonant = total_key(interval_consonant_count, (octave, n, d))
			dissonant = total_key(interval_dissonant_count, (octave, n, d))
			path      = total_key(interval_path_count,      (octave, n, d))
			print("%i %i/%i percent just:      %f" % (octave, n, d, float(just)      * 100 / total), file=self.report_file)
			print("%i %i/%i percent consonant: %f" % (octave, n, d, float(consonant) * 100 / total), file=self.report_file)
			print("%i %i/%i percent dissonant: %f" % (octave, n, d, float(dissonant) * 100 / total), file=self.report_file)
			print("%i %i/%i percent obscured:  %f" % (octave, n, d, float(total - consonant - dissonant) * 100 / total), file=self.report_file)
			print("%i %i/%i average path:      %f" % (octave, n, d, float(path) / total), file=self.report_file)
			print("%i %i/%i total:             %i" % (octave, n, d, total), file=self.report_file)

		# Aggregate report on the 5ths from each of the 12 notes in all octaves
		print("\nFifths from each note in all octaves (MIDI 0-127):", file=self.report_file)
		note_stats = {i: [] for i in range(12)}  # key: pitch class, value: list of (octave, ratio, cents)
		for n in range(0, 128):
			note = self.notes[n]
			if note.f is not None:
				try:
					fifth_note = note.get_interval(7)
					if fifth_note.f is not None:
						ratio = fifth_note.f / note.f
						cents = 1200.0 * math.log(ratio / (3/2)) / math.log(2)
						pitch_class = n % 12
						octave = n // 12 - 1
						note_stats[pitch_class].append((octave, ratio, cents))
					# else: skip if fifth frequency unknown
				except Exception:
					pass  # skip if out of range or error
		for pitch_class in range(12):
			name = NOTE_NAMES[pitch_class]
			stats = note_stats[pitch_class]
			if stats:
				avg_cents = sum(c for _, _, c in stats) / len(stats)
				print(f"\n{name}: {len(stats)} fifths found", file=self.report_file)
				for octave, ratio, cents in stats:
					print(
						f"{name}{octave}: actual ratio {ratio:.6f}, diff from pure 3/2: {cents:+.2f} cents",
						file=self.report_file
					)
				print(f"{name}: average deviation from pure 3/2: {avg_cents:+.2f} cents", file=self.report_file)
			else:
				print(f"\n{name}: no fifths found", file=self.report_file)

		# Aggregate report on the Major 3rds from each of the 12 notes in all octaves
		print("\nMajor 3rds from each note in all octaves (MIDI 0-127):", file=self.report_file)
		major3rd_stats = {i: [] for i in range(12)}  # key: pitch class, value: list of (octave, ratio, cents)
		for n in range(0, 128):
			note = self.notes[n]
			if note.f is not None:
				try:
					third_note = note.get_interval(4)
					if third_note.f is not None:
						ratio = third_note.f / note.f
						cents = 1200.0 * math.log(ratio / (5/4)) / math.log(2)
						pitch_class = n % 12
						octave = n // 12 - 1
						major3rd_stats[pitch_class].append((octave, ratio, cents))
				except Exception:
					pass  # skip if out of range or error
		for pitch_class in range(12):
			name = NOTE_NAMES[pitch_class]
			stats = major3rd_stats[pitch_class]
			if stats:
				avg_cents = sum(c for _, _, c in stats) / len(stats)
				print(f"\n{name}: {len(stats)} major 3rds found", file=self.report_file)
				for octave, ratio, cents in stats:
					print(
						f"{name}{octave}: actual ratio {ratio:.6f}, diff from pure 5/4: {cents:+.2f} cents",
						file=self.report_file
					)
				print(f"{name}: average deviation from pure 5/4: {avg_cents:+.2f} cents", file=self.report_file)
			else:
				print(f"\n{name}: no major 3rds found", file=self.report_file)


	def describe_sequence(self, file = None):
		"""
		Print a human-readable description of each note's tuning step 
		in graph order, starting from root notes and following child links.
		"""
		if file is not None:
			self.report_file = file

		for note in self.sequence:
			if note.f is not None:
				parent = note.parent
				if parent is not None:
					interval = note.n - parent.n
					ratio = note.f / parent.f if parent.f else None
					print(
						f"{midi_to_name(parent.n):>4} ({parent.f:8.2f} Hz) -> "
						f"{midi_to_name(note.n):>4} ({note.f:8.2f} Hz), "
						f"interval {interval:+3d}, ratio {ratio:8.5f}"
						, file=self.report_file
					)
				else:
					print(f"{midi_to_name(note.n):>4} ({note.f:8.2f} Hz) (reference note)", file=self.report_file)
			else:
				print(f"{midi_to_name(note.n):>4} (frequency unknown)", file=self.report_file)

	def describe_sequence_markdown(self, file = None):
		"""
		Print a human-readable description of each note's tuning step 
		in graph order, starting from root notes and following child links.
		"""
		if file is not None:
			self.report_file = file

		print("| Parent Note | Child Note | Interval | Ratio |", file=self.report_file)
		print("|-------------|------------|----------|-------|", file=self.report_file)

		for note in self.sequence:
			if note.f is not None:
				parent = note.parent
				if parent is not None:
					interval = note.n - parent.n
					ratio = note.f / parent.f if parent.f else None
					print(
						f"| {midi_to_name(parent.n):>4} ({parent.f:8.2f} Hz) | "
						f"{midi_to_name(note.n):>4} ({note.f:8.2f} Hz) | "
						f"{interval:+3d} | {ratio:8.5f} |"
						, file=self.report_file
					)
				else:
					print(f"| {midi_to_name(note.n):>4} ({note.f:8.2f} Hz) | (reference note) | | |", file=self.report_file)
			else:
				print(f"| {midi_to_name(note.n):>4} (frequency unknown) | | | |", file=self.report_file)


class ANotes(Notes):
	def __init__(self):
		Notes.__init__(self)
		C4 = self[60]
		# anchor C to 256 Hz
		C4.set_frequency(256)

		self.init_leaf(C4)

		up1 = C4.get_interval(21)
		# anchor leaf from octave below
		up1.rel_tune(-12, 2, 1)

		self.init_leaf(up1)

		up2 = C4.get_interval(42)
		# anchor leaf from octave below
		up2.rel_tune(-12, 2, 1)

		self.init_leaf(up2)

		down1 = C4.get_interval(-21)
		# anchor leaf from 3 octaves above
		down1.rel_tune(36, 1, 8)

		self.init_leaf(down1)

		down2 = C4.get_interval(-42)
		# anchor leaf from 3 octaves above
		down2.rel_tune(36, 1, 8)

		self.init_leaf(down2)
			

	def init_leaf(self, note):
		start = note

		for x in range(7):
			note = note.seq_tune(7, 3, 2)
			if x < 6: 
				# not the last time
				note = note.seq_tune(7, 3, 2)

			if x == 1: # second time
				# anchor the next sequence by 5/4 against `start`
				note = start.seq_tune(4, 5, 4)
			elif x < 6: # not the last time
				# anchor the next sequence down an octave from `k`
				note = note.seq_tune(-12, 1, 2)

class EqualNotes(Notes):
	def __init__(self):
		Notes.__init__(self)

		for note in self.notes:
			note.f = 256.0 * 2.0 ** ((note.n - 60) / 12.0)

class EqualPythagorean(Notes):
	def __init__(self):
		self.octave = 2.0 * stretch_interval
		Notes.__init__(self)

		for note in self.notes:
			note.f = 256.0 * (2.0 * stretch_interval) ** ((note.n - 60) / 12.0)


class TemperamentNotes(Notes):

	octave = 2.0

	def __init__(self):
		Notes.__init__(self)
		C4 = self[60]
		# anchor C to 256 Hz
		C4.set_frequency(256)

		self.init_temperament(C4)

		self.extrapolate_temperaments()

	def extrapolate_temperament_octave(self, from_octave, to_octave):
		shift_octave = to_octave - from_octave
		for i in range(12):
			note = self[60 + 12 * (from_octave - 4) + i]
			note = note.seq_tune(shift_octave * 12, self.octave ** shift_octave, 1)

	def extrapolate_temperaments(self):
		self.extrapolate_temperament_octave(4, 5)
		self.extrapolate_temperament_octave(5, 6)
		self.extrapolate_temperament_octave(6, 7)
		self.extrapolate_temperament_octave(4, 3)
		self.extrapolate_temperament_octave(3, 2)
		self.extrapolate_temperament_octave(2, 1)


class JustNotes(TemperamentNotes):
	def init_temperament(self, C4):
		# 3 terms
		F4  = C4.seq_tune(5, 4, 3)  # 3 term -1
		C4                          # 3 term  0
		G4  = C4.seq_tune(7, 3, 2)  # 3 term +1
		D4  = G4.seq_tune(-5, 3, 4) # 3 term +2

		# 5 term +1
		A4  = F4.seq_tune(4, 5, 4)  # 3 term -1
		E4  = C4.seq_tune(4, 5, 4)  # 3 term  0
		B4  = G4.seq_tune(4, 5, 4)  # 3 term +1
		Fs4 = D4.seq_tune(4, 5, 4)  # 3 term +2

		# 5 term -1
		Db4 = F4.seq_tune(-4,   4,   5) # 3 term -1
		Ab4 = C4.seq_tune(12-4, 4*2, 5) # 3 term  0
		Eb4 = G4.seq_tune(-4,   4,   5) # 3 term +1
		Bb4 = D4.seq_tune(12-4, 4*2, 5) # 3 term +2
				
class StretchJustNotes(TemperamentNotes):
	octave = stretch_octave

	def init_temperament(self, C4):
		# 3 terms
		F4  = C4.seq_tune(12-7, stretch_octave * 2, 3)  # 3 term -1
		C4                          # 3 term  0
		G4  = C4.seq_tune(7, 3, 2)  # 3 term +1
		D4  = G4.seq_tune(7 - 12, 3, 2 * stretch_octave) # 3 term +2

		# 5 term +1
		A4  = F4.seq_tune(4, 5, 4)  # 3 term -1
		E4  = C4.seq_tune(4, 5, 4)  # 3 term  0
		B4  = G4.seq_tune(4, 5, 4)  # 3 term +1
		Fs4 = D4.seq_tune(4, 5, 4)  # 3 term +2

		# 5 term -1
		Db4 = F4.seq_tune(-4,   4,   5) # 3 term -1
		Ab4 = C4.seq_tune(12-4, stretch_octave * 4, 5) # 3 term  0
		Eb4 = G4.seq_tune(-4,   4,   5) # 3 term +1
		Bb4 = D4.seq_tune(12-4, stretch_octave * 4, 5) # 3 term +2
				
		
class HybridNotes(Notes):
	def __init__(self):
		Notes.__init__(self)

		self.init_leaf_middle()

		self.init_octaves_low()

		self.init_octaves_high()

	def init_leaf_middle(self):
		# anchor C where A = 440 Hz
		C4 = self[60]
		C4.set_frequency(440.0 * 2 ** (3 / 12.0) / 2)

		# Tune a chain of 5ths from Bb2 to A6, out from C4 in two directions
		F3 = C4.seq_tune(-7, 2, 3)
		Bb2 = F3.seq_tune(-7, 2, 3)

		G4 = C4.seq_tune(7, 3, 2)
		D5 = G4.seq_tune(7, 3, 2)
		A5 = D5.seq_tune(7, 3, 2)

		# Fill in the notes between Bb2 and A6 by connecting octaves along their natural inharmonicity
		# from C4
		C5 = C4.seq_tune(12, 2, 1, True)
		C3 = C4.seq_tune(-12, 1, 2, True)
		
		# from F3
		F4 = F3.seq_tune(12, 2, 1, True)
		F5 = F4.seq_tune(12, 2, 1, True)

		# from Bb2
		Bb3 = Bb2.seq_tune(12, 2, 1, True)
		Bb4 = Bb3.seq_tune(12, 2, 1, True)

		# from G4
		G3 = G4.seq_tune(-12, 1, 2, True)
		G5 = G4.seq_tune(12, 2, 1, True)

		# from D5
		D4 = D5.seq_tune(-12, 1, 2, True)
		D3 = D4.seq_tune(-12, 1, 2, True)

		# from A5
		A4 = A5.seq_tune(-12, 1, 2, True)
		A3 = A4.seq_tune(-12, 1, 2, True)

		# notes currently in the middle of the keyboard
		# Bb2, Bb3, Bb4
		# F3, F4, F5
		# C3, C4, C5
		# G3, G4, G5
		# D3, D4, D5
		# A3, A4, A5

		# notes remaining in the middle of the keyboard
		# E3, E4, E5 # bridge between C4 and B2

		# B2, B3, B4
		# F#3, F#4, F#5
		# C#3, C#4, C#5
		# G#3, G#4, G#5
		# D#3, D#4, D#5


		# Tune a chain of 5ths from E4 (via E3 stretched octave, to B2 rational 4th)
		# Tune E4 rationally to C4
		E4 = C4.seq_tune(4, 5, 4)

		# Get to E3 from E4 stretched octave
		E3 = E4.seq_tune(-12, 1, 2, True)

		# Get to B3 from E3 rational 4th
		B2 = E3.seq_tune(-5, 3, 4)

		# chain B2 to D#5 via 5ths
		Fs3 = B2.seq_tune(7, 3, 2)
		Cs4 = Fs3.seq_tune(7, 3, 2)
		Gs4 = Cs4.seq_tune(7, 3, 2)
		Ds5 = Gs4.seq_tune(7, 3, 2)

		# Fill in the notes between B2 and D#5 by connecting octaves along their natural inharmonicity
		# from E4
		E5 = E4.seq_tune(12, 2, 1, True)
		# E3 is already set

		# from B2
		B3 = B2.seq_tune(12, 2, 1, True)
		B4 = B3.seq_tune(12, 2, 1, True)

		# from Fs3
		Fs4 = Fs3.seq_tune(12, 2, 1, True)
		Fs5 = Fs4.seq_tune(12, 2, 1, True)

		# from Cs4
		Cs5 = Cs4.seq_tune(12, 2, 1, True)
		Cs3 = Cs4.seq_tune(-12, 1, 2, True)

		# from Gs4
		Gs5 = Gs4.seq_tune(12, 2, 1, True)
		Gs3 = Gs4.seq_tune(-12, 1, 2, True)

		# from Ds5
		Ds4 = Ds5.seq_tune(-12, 1, 2, True)
		Ds3 = Ds4.seq_tune(-12, 1, 2, True)

		


	def init_octaves_low(self):
		# stretched octaves down from below Bb2

		# Bb2 - A3 references
		Bb2 = self[60 - 12 - 1]
		B2  = self[60 - 12 - 2]
		C3  = self[60 - 12]
		Cs3 = self[60 - 12 + 1]
		D3  = self[60 - 12 + 2]
		Ds3 = self[60 - 12 + 3]
		E3  = self[60 - 12 + 4]
		F3  = self[60 - 12 + 5]
		Fs3 = self[60 - 12 + 6]
		G3  = self[60 - 12 + 7]
		Gs3 = self[60 - 12 + 8]
		A3  = self[60 - 12 + 9]

		# Bb1 - A2
		Bb1 = Bb2.seq_tune(-12, 1, 2, True)
		B1  = B2.seq_tune(-12, 1, 2, True)
		C2  = C3.seq_tune(-12, 1, 2, True)
		Cs2 = Cs3.seq_tune(-12, 1, 2, True)
		D2  = D3.seq_tune(-12, 1, 2, True)
		Ds2 = Ds3.seq_tune(-12, 1, 2, True)
		E2  = E3.seq_tune(-12, 1, 2, True)
		F2  = F3.seq_tune(-12, 1, 2, True)
		Fs2 = Fs3.seq_tune(-12, 1, 2, True)
		G2  = G3.seq_tune(-12, 1, 2, True)
		Gs2 = Gs3.seq_tune(-12, 1, 2, True)
		A2  = A3.seq_tune(-12, 1, 2, True)

		# Bb0 - A1
		Bb0 = Bb1.seq_tune(-12, 1, 2, True)
		B0  = B1.seq_tune(-12, 1, 2, True)
		C1  = C2.seq_tune(-12, 1, 2, True)
		Cs1 = Cs2.seq_tune(-12, 1, 2, True)
		D1  = D2.seq_tune(-12, 1, 2, True)
		Ds1 = Ds2.seq_tune(-12, 1, 2, True)
		E1  = E2.seq_tune(-12, 1, 2, True)
		F1  = F2.seq_tune(-12, 1, 2, True)
		Fs1 = Fs2.seq_tune(-12, 1, 2, True)
		G1  = G2.seq_tune(-12, 1, 2, True)
		Gs1 = Gs2.seq_tune(-12, 1, 2, True)
		A1  = A2.seq_tune(-12, 1, 2, True)

		# A0
		A0  = A1.seq_tune(-12, 1, 2, True)

	def init_octaves_high(self):
		# stretched octaves up from above A5
		# Bb4 - A5 references
		Bb4 = self[60 + 12 - 2]
		B4  = self[60 + 12 - 1]
		C5  = self[60 + 12]
		Cs5 = self[60 + 12 + 1]
		D5  = self[60 + 12 + 2]
		Ds5 = self[60 + 12 + 3]
		E5  = self[60 + 12 + 4]
		F5  = self[60 + 12 + 5]
		Fs5 = self[60 + 12 + 6]
		G5  = self[60 + 12 + 7]
		Gs5 = self[60 + 12 + 8]
		A5  = self[60 + 12 + 9]

		# Bb5 - A6
		Bb5 = Bb4.seq_tune(12, 2, 1, True)
		B5  = B4.seq_tune(12, 2, 1, True)
		C6  = C5.seq_tune(12, 2, 1, True)
		Cs6 = Cs5.seq_tune(12, 2, 1, True)
		D6  = D5.seq_tune(12, 2, 1, True)
		Ds6 = Ds5.seq_tune(12, 2, 1, True)
		E6  = E5.seq_tune(12, 2, 1, True)
		F6  = F5.seq_tune(12, 2, 1, True)
		Fs6 = Fs5.seq_tune(12, 2, 1, True)
		G6  = G5.seq_tune(12, 2, 1, True)
		Gs6 = Gs5.seq_tune(12, 2, 1, True)
		A6  = A5.seq_tune(12, 2, 1, True)

		# Bb6 - A7
		Bb6 = Bb5.seq_tune(12, 2, 1, True)
		B6  = B5.seq_tune(12, 2, 1, True)
		C7  = C6.seq_tune(12, 2, 1, True)
		Cs7 = Cs6.seq_tune(12, 2, 1, True)
		D7  = D6.seq_tune(12, 2, 1, True)
		Ds7 = Ds6.seq_tune(12, 2, 1, True)
		E7  = E6.seq_tune(12, 2, 1, True)
		F7  = F6.seq_tune(12, 2, 1, True)
		Fs7 = Fs6.seq_tune(12, 2, 1, True)
		G7  = G6.seq_tune(12, 2, 1, True)
		Gs7 = Gs6.seq_tune(12, 2, 1, True)
		A7  = A6.seq_tune(12, 2, 1, True)
		
		# Bb7 - C8
		Bb7 = Bb6.seq_tune(12, 2, 1, True)
		B7  = B6.seq_tune(12, 2, 1, True)
		C8  = C7.seq_tune(12, 2, 1, True)

class SemiNotes(Notes):
	def __init__(self):
		Notes.__init__(self)

		self.init_leaf_middle()

		self.init_octaves_low()

		self.init_octaves_high()

	def init_leaf_middle(self):
		# anchor C where A = 440 Hz
		C4 = self[60]
		C4.set_frequency(440.0 * 2 ** (3 / 12.0) / 2)

		# Tune a chain of 5ths from Bb2 to A6, out from C4 in two directions
		F3 = C4.seq_tune(-7, 2, 3)
		Bb2 = F3.seq_tune(-7, 2, 3)

		G4 = C4.seq_tune(7, 3, 2)
		D5 = G4.seq_tune(7, 3, 2)
		A5 = D5.seq_tune(7, 3, 2)

		# Fill in the notes between Bb2 and A6 by connecting octaves along their natural inharmonicity
		# from C4
		C5 = C4.seq_tune(12, 2, 1, True)
		C3 = C4.seq_tune(-12, 1, 2, True)
		
		# from F3
		F4 = F3.seq_tune(12, 2, 1, True)
		F5 = F4.seq_tune(12, 2, 1, True)

		# from Bb2
		Bb3 = Bb2.seq_tune(12, 2, 1, True)
		Bb4 = Bb3.seq_tune(12, 2, 1, True)

		# from G4
		G3 = G4.seq_tune(-12, 1, 2, True)
		G5 = G4.seq_tune(12, 2, 1, True)

		# from D5
		D4 = D5.seq_tune(-12, 1, 2, True)
		D3 = D4.seq_tune(-12, 1, 2, True)

		# from A5
		A4 = A5.seq_tune(-12, 1, 2, True)
		A3 = A4.seq_tune(-12, 1, 2, True)

		# notes currently in the middle of the keyboard
		# Bb2, Bb3, Bb4
		# F3, F4, F5
		# C3, C4, C5
		# G3, G4, G5
		# D3, D4, D5
		# A3, A4, A5

		# notes remaining in the middle of the keyboard
		# E3, E4, E5 # bridge between C4 and B2

		# B2, B3, B4
		# F#3, F#4, F#5
		# C#3, C#4, C#5
		# G#3, G#4, G#5
		# D#3, D#4, D#5


		# Tune a chain of 5ths from E4 (via E3 stretched octave, to B2 rational 4th)
		# Tune E4 rationally to C4
		#E4 = C4.seq_tune(4, 5, 4)

		# Tune E4 via 5ths from A2
		#A2 = A3.seq_tune(-12, 1, 2, True)
		#E4 = A2.seq_tune(7, 3, 2)

		# Tune E4 via 5th from A3
		E4 = A3.seq_tune(7, 3, 2)

		# Get to E3 from E4 stretched octave
		E3 = E4.seq_tune(-12, 1, 2, True)

		# Get to B3 from E3 rational 4th
		B2 = E3.seq_tune(-5, 3, 4)

		# chain B2 to D#5 via 5ths
		Fs3 = B2.seq_tune(7, 3, 2)
		Cs4 = Fs3.seq_tune(7, 3, 2)
		Gs4 = Cs4.seq_tune(7, 3, 2)
		Ds5 = Gs4.seq_tune(7, 3, 2)

		# Fill in the notes between B2 and D#5 by connecting octaves along their natural inharmonicity
		# from E4
		E5 = E4.seq_tune(12, 2, 1, True)
		# E3 is already set

		# from B2
		B3 = B2.seq_tune(12, 2, 1, True)
		B4 = B3.seq_tune(12, 2, 1, True)

		# from Fs3
		Fs4 = Fs3.seq_tune(12, 2, 1, True)
		Fs5 = Fs4.seq_tune(12, 2, 1, True)

		# from Cs4
		Cs5 = Cs4.seq_tune(12, 2, 1, True)
		Cs3 = Cs4.seq_tune(-12, 1, 2, True)

		# from Gs4
		Gs5 = Gs4.seq_tune(12, 2, 1, True)
		Gs3 = Gs4.seq_tune(-12, 1, 2, True)

		# from Ds5
		Ds4 = Ds5.seq_tune(-12, 1, 2, True)
		Ds3 = Ds4.seq_tune(-12, 1, 2, True)

		


	def init_octaves_low(self):
		# stretched octaves down from below Bb2

		# Bb2 - A3 references
		Bb2 = self[60 - 12 - 1]
		B2  = self[60 - 12 - 2]
		C3  = self[60 - 12]
		Cs3 = self[60 - 12 + 1]
		D3  = self[60 - 12 + 2]
		Ds3 = self[60 - 12 + 3]
		E3  = self[60 - 12 + 4]
		F3  = self[60 - 12 + 5]
		Fs3 = self[60 - 12 + 6]
		G3  = self[60 - 12 + 7]
		Gs3 = self[60 - 12 + 8]
		A3  = self[60 - 12 + 9]

		# Bb1 - A2
		Bb1 = Bb2.seq_tune(-12, 1, 2, True)
		B1  = B2.seq_tune(-12, 1, 2, True)
		C2  = C3.seq_tune(-12, 1, 2, True)
		Cs2 = Cs3.seq_tune(-12, 1, 2, True)
		D2  = D3.seq_tune(-12, 1, 2, True)
		Ds2 = Ds3.seq_tune(-12, 1, 2, True)
		E2  = E3.seq_tune(-12, 1, 2, True)
		F2  = F3.seq_tune(-12, 1, 2, True)
		Fs2 = Fs3.seq_tune(-12, 1, 2, True)
		G2  = G3.seq_tune(-12, 1, 2, True)
		Gs2 = Gs3.seq_tune(-12, 1, 2, True)
		A2  = A3.seq_tune(-12, 1, 2, True)

		# Bb0 - A1
		Bb0 = Bb1.seq_tune(-12, 1, 2, True)
		B0  = B1.seq_tune(-12, 1, 2, True)
		C1  = C2.seq_tune(-12, 1, 2, True)
		Cs1 = Cs2.seq_tune(-12, 1, 2, True)
		D1  = D2.seq_tune(-12, 1, 2, True)
		Ds1 = Ds2.seq_tune(-12, 1, 2, True)
		E1  = E2.seq_tune(-12, 1, 2, True)
		F1  = F2.seq_tune(-12, 1, 2, True)
		Fs1 = Fs2.seq_tune(-12, 1, 2, True)
		G1  = G2.seq_tune(-12, 1, 2, True)
		Gs1 = Gs2.seq_tune(-12, 1, 2, True)
		A1  = A2.seq_tune(-12, 1, 2, True)

		# A0
		A0  = A1.seq_tune(-12, 1, 2, True)

	def init_octaves_high(self):
		# stretched octaves up from above A5
		# Bb4 - A5 references
		Bb4 = self[60 + 12 - 2]
		B4  = self[60 + 12 - 1]
		C5  = self[60 + 12]
		Cs5 = self[60 + 12 + 1]
		D5  = self[60 + 12 + 2]
		Ds5 = self[60 + 12 + 3]
		E5  = self[60 + 12 + 4]
		F5  = self[60 + 12 + 5]
		Fs5 = self[60 + 12 + 6]
		G5  = self[60 + 12 + 7]
		Gs5 = self[60 + 12 + 8]
		A5  = self[60 + 12 + 9]

		# Bb5 - A6
		Bb5 = Bb4.seq_tune(12, 2, 1, True)
		B5  = B4.seq_tune(12, 2, 1, True)
		C6  = C5.seq_tune(12, 2, 1, True)
		Cs6 = Cs5.seq_tune(12, 2, 1, True)
		D6  = D5.seq_tune(12, 2, 1, True)
		Ds6 = Ds5.seq_tune(12, 2, 1, True)
		E6  = E5.seq_tune(12, 2, 1, True)
		F6  = F5.seq_tune(12, 2, 1, True)
		Fs6 = Fs5.seq_tune(12, 2, 1, True)
		G6  = G5.seq_tune(12, 2, 1, True)
		Gs6 = Gs5.seq_tune(12, 2, 1, True)
		A6  = A5.seq_tune(12, 2, 1, True)

		# Bb6 - A7
		Bb6 = Bb5.seq_tune(12, 2, 1, True)
		B6  = B5.seq_tune(12, 2, 1, True)
		C7  = C6.seq_tune(12, 2, 1, True)
		Cs7 = Cs6.seq_tune(12, 2, 1, True)
		D7  = D6.seq_tune(12, 2, 1, True)
		Ds7 = Ds6.seq_tune(12, 2, 1, True)
		E7  = E6.seq_tune(12, 2, 1, True)
		F7  = F6.seq_tune(12, 2, 1, True)
		Fs7 = Fs6.seq_tune(12, 2, 1, True)
		G7  = G6.seq_tune(12, 2, 1, True)
		Gs7 = Gs6.seq_tune(12, 2, 1, True)
		A7  = A6.seq_tune(12, 2, 1, True)
		
		# Bb7 - C8
		Bb7 = Bb6.seq_tune(12, 2, 1, True)
		B7  = B6.seq_tune(12, 2, 1, True)
		C8  = C7.seq_tune(12, 2, 1, True)

class SpiralNotes(Notes):
	def __init__(self):
		Notes.__init__(self)

		self.init_leaf_middle()

		self.init_octaves_low()

		self.init_octaves_high()

	def init_leaf_middle(self):
		# anchor C where A = 440 Hz
		C4 = self[60]
		C4.set_frequency(440.0 * 2 ** (3 / 12.0) / 2)

		# Tune a chain of 5ths from Bb2 to A6, out from C4 in two directions
		F3 = C4.seq_tune(-7, 2, 3)
		Bb2 = F3.seq_tune(-7, 2, 3)

		G4 = C4.seq_tune(7, 3, 2)
		D5 = G4.seq_tune(7, 3, 2)
		A5 = D5.seq_tune(7, 3, 2)

		# Fill in the notes between Bb2 and A6 by connecting octaves along their natural inharmonicity
		# from C4
		C5 = C4.seq_tune(12, 2, 1, True)
		C3 = C4.seq_tune(-12, 1, 2, True)
		
		# from F3
		F4 = F3.seq_tune(12, 2, 1, True)
		F5 = F4.seq_tune(12, 2, 1, True)

		# from Bb2
		Bb3 = Bb2.seq_tune(12, 2, 1, True)
		Bb4 = Bb3.seq_tune(12, 2, 1, True)

		# from G4
		G3 = G4.seq_tune(-12, 1, 2, True)
		G5 = G4.seq_tune(12, 2, 1, True)

		# from D5
		D4 = D5.seq_tune(-12, 1, 2, True)
		D3 = D4.seq_tune(-12, 1, 2, True)

		# from A5
		A4 = A5.seq_tune(-12, 1, 2, True)
		A3 = A4.seq_tune(-12, 1, 2, True)

		# notes currently in the middle of the keyboard
		# .Bb2, Bb3, Bb4
		# .F3,  F4,  F5
		#  C3, .C4,  C5
		#  G3, .G4,  G5
		#  D3,  D4, .D5
		#  A3,  A4, .A5

		# notes remaining in the middle of the keyboard
		# .E3,   E4,   E5
		#  B2,  .B3,   B4
		#  F#3, .F#4,  F#5
		#  C#3,  C#4, .C#5
		#  G#3,  G#4, .G#5
		#  D#3,  D#4, .D#5


		# Tune a chain of 5ths from A2 to D#5, out from A3
		A2 = A3.seq_tune(-12, 1, 2, True)
		E3 = A2.seq_tune(7, 3, 2)
		B3 = E3.seq_tune(7, 3, 2)
		Fs4 = B3.seq_tune(7, 3, 2)
		Cs5 = Fs4.seq_tune(7, 3, 2)
		Gs5 = Cs5.seq_tune(7, 3, 2)
		Ds5 = Gs5.seq_tune(-5, 3, 4)

		# Fill in the notes between A2 and D#5 by connecting octaves along their natural inharmonicity
		# from E3
		E4 = E3.seq_tune(12, 2, 1, True)
		E5 = E4.seq_tune(12, 2, 1, True)

		# from B3
		B4 = B3.seq_tune(12, 2, 1, True)
		B2 = B3.seq_tune(-12, 1, 2, True)

		# from Fs4
		Fs5 = Fs4.seq_tune(12, 2, 1, True)
		Fs3 = Fs4.seq_tune(-12, 1, 2, True)

		# from Cs5
		Cs4 = Cs5.seq_tune(-12, 1, 2, True)
		Cs3 = Cs4.seq_tune(-12, 1, 2, True)

		# from Gs5
		Gs4 = Gs5.seq_tune(-12, 1, 2, True)
		Gs3 = Gs4.seq_tune(-12, 1, 2, True)

		# from Ds5
		Ds4 = Ds5.seq_tune(-12, 1, 2, True)
		Ds3 = Ds4.seq_tune(-12, 1, 2, True)
		


	def init_octaves_low(self):
		# stretched octaves down from below Bb2

		# Bb2 - A3 references
		Bb2 = self[60 - 12 - 1]
		B2  = self[60 - 12 - 2]
		C3  = self[60 - 12]
		Cs3 = self[60 - 12 + 1]
		D3  = self[60 - 12 + 2]
		Ds3 = self[60 - 12 + 3]
		E3  = self[60 - 12 + 4]
		F3  = self[60 - 12 + 5]
		Fs3 = self[60 - 12 + 6]
		G3  = self[60 - 12 + 7]
		Gs3 = self[60 - 12 + 8]
		A3  = self[60 - 12 + 9]

		# Bb1 - A2
		Bb1 = Bb2.seq_tune(-12, 1, 2, True)
		B1  = B2.seq_tune(-12, 1, 2, True)
		C2  = C3.seq_tune(-12, 1, 2, True)
		Cs2 = Cs3.seq_tune(-12, 1, 2, True)
		D2  = D3.seq_tune(-12, 1, 2, True)
		Ds2 = Ds3.seq_tune(-12, 1, 2, True)
		E2  = E3.seq_tune(-12, 1, 2, True)
		F2  = F3.seq_tune(-12, 1, 2, True)
		Fs2 = Fs3.seq_tune(-12, 1, 2, True)
		G2  = G3.seq_tune(-12, 1, 2, True)
		Gs2 = Gs3.seq_tune(-12, 1, 2, True)
		A2  = self[60 - 12 - 12 + 9]
		#A2  = A3.seq_tune(-12, 1, 2, True)

		# Bb0 - A1
		Bb0 = Bb1.seq_tune(-12, 1, 2, True)
		B0  = B1.seq_tune(-12, 1, 2, True)
		C1  = C2.seq_tune(-12, 1, 2, True)
		Cs1 = Cs2.seq_tune(-12, 1, 2, True)
		D1  = D2.seq_tune(-12, 1, 2, True)
		Ds1 = Ds2.seq_tune(-12, 1, 2, True)
		E1  = E2.seq_tune(-12, 1, 2, True)
		F1  = F2.seq_tune(-12, 1, 2, True)
		Fs1 = Fs2.seq_tune(-12, 1, 2, True)
		G1  = G2.seq_tune(-12, 1, 2, True)
		Gs1 = Gs2.seq_tune(-12, 1, 2, True)
		A1  = A2.seq_tune(-12, 1, 2, True)

		# A0
		A0  = A1.seq_tune(-12, 1, 2, True)

	def init_octaves_high(self):
		# stretched octaves up from above A5
		# Bb4 - A5 references
		Bb4 = self[60 + 12 - 2]
		B4  = self[60 + 12 - 1]
		C5  = self[60 + 12]
		Cs5 = self[60 + 12 + 1]
		D5  = self[60 + 12 + 2]
		Ds5 = self[60 + 12 + 3]
		E5  = self[60 + 12 + 4]
		F5  = self[60 + 12 + 5]
		Fs5 = self[60 + 12 + 6]
		G5  = self[60 + 12 + 7]
		Gs5 = self[60 + 12 + 8]
		A5  = self[60 + 12 + 9]

		# Bb5 - A6
		Bb5 = Bb4.seq_tune(12, 2, 1, True)
		B5  = B4.seq_tune(12, 2, 1, True)
		C6  = C5.seq_tune(12, 2, 1, True)
		Cs6 = Cs5.seq_tune(12, 2, 1, True)
		D6  = D5.seq_tune(12, 2, 1, True)
		Ds6 = Ds5.seq_tune(12, 2, 1, True)
		E6  = E5.seq_tune(12, 2, 1, True)
		F6  = F5.seq_tune(12, 2, 1, True)
		Fs6 = Fs5.seq_tune(12, 2, 1, True)
		G6  = G5.seq_tune(12, 2, 1, True)
		Gs6 = Gs5.seq_tune(12, 2, 1, True)
		A6  = A5.seq_tune(12, 2, 1, True)

		# Bb6 - A7
		Bb6 = Bb5.seq_tune(12, 2, 1, True)
		B6  = B5.seq_tune(12, 2, 1, True)
		C7  = C6.seq_tune(12, 2, 1, True)
		Cs7 = Cs6.seq_tune(12, 2, 1, True)
		D7  = D6.seq_tune(12, 2, 1, True)
		Ds7 = Ds6.seq_tune(12, 2, 1, True)
		E7  = E6.seq_tune(12, 2, 1, True)
		F7  = F6.seq_tune(12, 2, 1, True)
		Fs7 = Fs6.seq_tune(12, 2, 1, True)
		G7  = G6.seq_tune(12, 2, 1, True)
		Gs7 = Gs6.seq_tune(12, 2, 1, True)
		A7  = A6.seq_tune(12, 2, 1, True)
		
		# Bb7 - C8
		Bb7 = Bb6.seq_tune(12, 2, 1, True)
		B7  = B6.seq_tune(12, 2, 1, True)
		C8  = C7.seq_tune(12, 2, 1, True)





class PATHNotes(Notes):
	def __init__(self, use_inharmonicity = False, temper_thirds = False):
		self.use_inharmonicity = use_inharmonicity
		self.temper_thirds = temper_thirds
		Notes.__init__(self)

		self.init_leaf3()

		self.init_leaf4()

		self.init_leaf2()

		self.init_leaf5()

		self.init_leaf1()

	def init_leaf1(self):
		A1 = self[60 - 12 - 12 - 3]
		for note in [A1.get_interval(- i) for i in range(13)]:
			note.rel_tune(12, 1, 2, self.use_inharmonicity)
	
	def init_leaf2(self):
		F3 = self[60 - 7]
		F2 = F3.seq_tune(-12, 1, 2, self.use_inharmonicity)

		note = F2.seq_tune(-7, 2, 3)    # A#1
		note = F2.seq_tune(7, 3, 2)     # C3
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # C2
		note = note.seq_tune(7, 3, 2)   # G2
		note = note.seq_tune(7, 3, 2)   # D3

		A3 = self[60 - 3]
		A2 = A3.seq_tune(-12, 1, 2, self.use_inharmonicity) # A2

		note = A2.seq_tune(-7, 2, 3)    # D2
		note = A2.seq_tune(7, 3, 2)     # E3
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # E2
		note = note.seq_tune(7, 3, 2)   # B2
		
		Fs3 = self[60 - 6]
		Fs2 = Fs3.seq_tune(-12, 1, 2, self.use_inharmonicity)

		note = Fs2.seq_tune(-7, 2, 3)   # B1
		note = Fs2.seq_tune(7, 3, 2)    # C#3
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # C#2
		note = note.seq_tune(7, 3, 2)   # G#2
		note = note.seq_tune(7, 3, 2)   # D#3
		
		As3 = self[60 - 2]
		As2 = As3.seq_tune(-12, 1, 2, self.use_inharmonicity)

		note = As2.seq_tune(-7, 2, 3)   # D#2
		

	def init_leaf3(self):
		C4 = self[60]
		# anchor C to 256 Hz
		C4.set_frequency(256)

		# good Major 3rds by linking      E4 to C4              :         G3-B3 A3-C#4        C4-E4       D4-F#4         G4-B4 A4-C#5 
		# good Major 3rds by linking both E4 to C4 and D#4 to B3: F#3-A#3 G3-B3 A3-C#4 B4-D#4 C4-E4 C#4-F D4-F#4 F#4-A#4 G4-B4 A4-C#5

		note = C4.seq_tune(-7, 2, 3)    # F3
		note = C4.seq_tune(7, 3, 2)     # G4
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # G3
		note = note.seq_tune(7, 3, 2)   # D4
		note = note.seq_tune(7, 3, 2)   # A4
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # A3
		if self.temper_thirds:
			# use 5/4 from C4, rather than pure pythagorean via 5ths, to roll back a syntonic comma across the circle. (80/64 instead of 81/64)
			note = C4.seq_tune(4, 5, 4)     # E4
		else:
			note = note.seq_tune(7, 3, 2)   # E4
		note = note.seq_tune(7, 3, 2)   # B4
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # B3
		B3   = note
		note = note.seq_tune(7, 3, 2)   # F#4
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # F#3
		note = note.seq_tune(7, 3, 2)   # C#4
		note = note.seq_tune(7, 3, 2)   # G#4
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # G#3
		note = note.seq_tune(7, 3, 2)   # D#4
		# use 5/4 from B3
		#note = B3.seq_tune(4, 5, 4)     # D#4
		note = note.seq_tune(7, 3, 2)   # A#4
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # A#3
		note = note.seq_tune(7, 3, 2)   # F4

	def init_leaf4(self):
		G4 = self[60 + 7]
		G5 = G4.seq_tune(12, 2, 1, self.use_inharmonicity)

		note = G5.seq_tune(-7, 2, 3)    # C5
		note = G5.seq_tune(7, 3, 2)     # D6
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # D5
		note = note.seq_tune(7, 3, 2)   # A5
		note = note.seq_tune(7, 3, 2)   # E6

		E4 = self[60 + 4]
		E5 = E4.seq_tune(12, 2, 1, self.use_inharmonicity)

		if self.temper_thirds:
			# use 5/4 from C4, rather than pure pythagorean via 5ths, to roll back a syntonic comma across the circle. (80/64 instead of 81/64)
			note = G5.seq_tune(4, 5, 4)     # B5
		else:
			note = E5.seq_tune(7, 3, 2)     # B5
		note = note.seq_tune(7, 3, 2)   # F#6
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # F#5
		note = note.seq_tune(7, 3, 2)   # C#6

		Cs4 = self[60 + 1]
		Cs5 = Cs4.seq_tune(12, 2, 1, self.use_inharmonicity)

		note = Cs5.seq_tune(7, 3, 2)    # G#5
		note = note.seq_tune(7, 3, 2)   # D#6
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # D#5
		note = note.seq_tune(7, 3, 2)   # A#5
		note = note.seq_tune(7, 3, 2)   # F6

		F4 = self[60 + 5]
		F5 = F4.seq_tune(12, 2, 1, self.use_inharmonicity)

		note = F5.seq_tune(7, 3, 2)   # C6	

	def init_leaf5(self):
		D6 = self[60 + 12 + 12 + 2]
		D7 = D6.seq_tune(12, 2, 1, self.use_inharmonicity)

		note = D7.seq_tune(-7, 2, 3)    # G6
		note = D7.seq_tune(7, 3, 2)     # A7
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # A6
		note = note.seq_tune(7, 3, 2)   # E7
		note = note.seq_tune(7, 3, 2)   # B7

		B5 = self[60 + 12 + 11]
		B6 = B5.seq_tune(12, 2, 1, self.use_inharmonicity)

		note = B6.seq_tune(7, 3, 2)     # F#7
		# C#8 does not exist
		note = note.seq_tune(-5, 3, 4)  # C#7 (down a 4th, rather than up a 5th and down an 8th)
		note = note.seq_tune(7, 3, 2)   # G#7

		Gs5 = self[60 + 12 + 8]
		Gs6 = Gs5.seq_tune(12, 2, 1, self.use_inharmonicity)

		note = Gs6.seq_tune(7, 3, 2)    # D#7
		note = note.seq_tune(7, 3, 2)   # A#7
		note = note.seq_tune(-12, 1, 2, self.use_inharmonicity) # A#6
		note = note.seq_tune(7, 3, 2)   # F7
		note = note.seq_tune(7, 3, 2)   # C8

		C6 = self[60 + 12 + 12]
		C7 = C6.seq_tune(12, 2, 1, self.use_inharmonicity)

		note = C7.seq_tune(7, 3, 2)     # G7
		

def main():
	r = open("equal_report.txt", "w")
	s = open("equal_sequence.txt", "w")
	m = open("equal_markdown.md", "w")
	equalnotes = EqualNotes()
	equalnotes.describe_sequence(s)
	equalnotes.describe_sequence_markdown(m)
	equalnotes.report(r)

	r = open("equalpyth_report.txt", "w")
	s = open("equalpyth_sequence.txt", "w")
	m = open("equalpyth_markdown.md", "w")
	equalpythnotes = EqualPythagorean()
	equalpythnotes.describe_sequence(s)
	equalpythnotes.describe_sequence_markdown(m)
	equalpythnotes.report(r)

	r = open("a_report.txt", "w")
	s = open("a_sequence.txt", "w")
	m = open("a_markdown.md", "w")
	anotes = ANotes()
	anotes.describe_sequence(s)
	anotes.describe_sequence_markdown(m)
	anotes.report(r)

	r = open("path_report.txt", "w")
	s = open("path_sequence.txt", "w")
	m = open("path_markdown.md", "w")
	pathnotes = PATHNotes(True, True)
	pathnotes.describe_sequence(s)
	pathnotes.describe_sequence_markdown(m)
	pathnotes.report(r)

	r = open("hybrid_report.txt", "w")
	s = open("hybrid_sequence.txt", "w")
	m = open("hybrid_markdown.md", "w")
	hybridnotes = HybridNotes()
	hybridnotes.describe_sequence(s)
	hybridnotes.describe_sequence_markdown(m)
	hybridnotes.report(r)

	r = open("semi_report.txt", "w")
	s = open("semi_sequence.txt", "w")
	m = open("semi_markdown.md", "w")
	seminotes = SemiNotes()
	seminotes.describe_sequence(s)
	seminotes.describe_sequence_markdown(m)
	seminotes.report(r)

	r = open("spiral_report.txt", "w")
	s = open("spiral_sequence.txt", "w")
	m = open("spiral_markdown.md", "w")
	spiralnotes = SpiralNotes()
	spiralnotes.describe_sequence(s)
	spiralnotes.describe_sequence_markdown(m)
	spiralnotes.report(r)

	r = open("just_report.txt", "w")
	s = open("just_sequence.txt", "w")
	m = open("just_markdown.md", "w")
	justnotes = JustNotes()
	justnotes.describe_sequence(s)
	justnotes.describe_sequence_markdown(m)
	justnotes.report(r)

	r = open("stretch_report.txt", "w")
	s = open("stretch_sequence.txt", "w")
	m = open("stretch_markdown.md", "w")
	stretchjustnotes = StretchJustNotes()
	stretchjustnotes.describe_sequence(s)
	stretchjustnotes.describe_sequence_markdown(m)
	stretchjustnotes.report(r)

if __name__ == "__main__":
	main()
