
############################################
### NMSEBC anonymization script
### written by chris koops may 2012 for catherine travis
### has various idiosyncracies (e.g. assumes relevant TextGrid tier is the top one).
### requires a Sound and accompanying TextGrid to be already loaded into Praat
### will write a potentially huge file your HD
############################################

form Enter directory name, including final slash
    comment Enter directory where anonymized file will be saved:
    sentence Directory /Users/grantberry/Desktop/ANON/
    comment Before you hit 'OK', make sure you have two files loaded into Praat:
    comment (1) the original recording loaded as a LongSound
    comment (2) accompanying TextGrid with all segments to be anonymized labeled "a"
endform

select all
file$ = selected$ ("LongSound")
grid$ = selected$ ("TextGrid")

select LongSound 'file$'
To TextGrid... boundaries 
Rename... new_boundaries

select TextGrid 'grid$'
intervals = Get number of intervals... 1
for n from 1 to intervals
	select TextGrid 'grid$'
	beg = Get start point... 1 n
	end = Get end point... 1 n
	dur = end-beg
	new_boundaries = round(dur/200)-1
	if new_boundaries > 0
		for b from 1 to new_boundaries
			select TextGrid new_boundaries
			new_boundary = beg+b*(dur/(new_boundaries+1))
			Insert boundary... 1 new_boundary
			endfor
		endif
	endfor

select TextGrid new_boundaries
new_boundaries = Get number of intervals... 1
new_boundaries = new_boundaries-1
if new_boundaries > 0
	for n from 1 to new_boundaries
		select TextGrid new_boundaries
		new_boundary = Get end point... 1 n
		select TextGrid 'grid$'
		Insert boundary... 1 new_boundary
		endfor
	endif
select TextGrid new_boundaries
Remove

beg = 0
margin = 0.025

select TextGrid 'grid$'
intervals = Get number of intervals... 1
for n from 1 to intervals
	label RESTART
	select TextGrid 'grid$'
	end = Get end point... 1 n
	label$ = Get label of interval... 1 n
	if n > 1
		prevlabel$ = Get label of interval... 1 n-1
	elif n = 1
		prevlabel$ = ""
		endif
	if n < intervals
		nextlabel$ = Get label of interval... 1 n+1
	elif n = intervals
		nextlabel$ = ""
		endif
	interval_dur = end-beg
	interval_dur_rounded = 'interval_dur:1'

	if label$ = "a"
		select LongSound 'file$'
		Extract part... beg beg+2*margin yes
		Rename... delete_1
		Extract part... beg beg+2*margin parabolic 1 yes
		Rename... delete_2
		Extract part... beg beg+margin rectangular 1 yes
		Rename... beg_piece
		select LongSound 'file$'
		Extract part... beg+margin end-margin yes
		Rename... middle_piece
		select LongSound 'file$'
		Extract part... end-2*margin end yes
		Rename... delete_3
		Extract part... end-2*margin end parabolic 1 yes
		Rename... delete_4
		Extract part... end-margin end rectangular 1 yes
		Rename... end_piece
		select Sound delete_1
		plus Sound delete_2
		plus Sound delete_3
		plus Sound delete_4
		Remove
		select Sound beg_piece
		plus Sound middle_piece
		plus Sound end_piece
		Concatenate
		Rename... interval_'n'
		select Sound beg_piece
		plus Sound middle_piece
		plus Sound end_piece
		Remove
		select Sound interval_'n'
		Filter (pass Hann band)... 0 500 50
		Rename... interval_'n'_lowpassed
		select Sound interval_'n'
		Remove
		select Sound interval_'n'_lowpassed
		Rename... interval_'n'

	elif label$ = "" and prevlabel$ = "a" and nextlabel$ = "a"
		select LongSound 'file$'
		Extract part... beg beg+2*margin yes
		Rename... delete_1
		Extract part... beg beg+2*margin parabolic 1 yes
		Rename... delete_2
		Extract part... beg beg+margin rectangular 1 yes
		Rename... beg_piece
		select LongSound 'file$'
		Extract part... beg+margin end-margin yes
		Rename... middle_piece
		select LongSound 'file$'
		Extract part... end-2*margin end yes
		Rename... delete_3
		Extract part... end-2*margin end parabolic 1 yes
		Rename... delete_4
		Extract part... end-margin end rectangular 1 yes
		Rename... end_piece
		select Sound delete_1
		plus Sound delete_2
		plus Sound delete_3
		plus Sound delete_4
		Remove
		select Sound beg_piece
		plus Sound middle_piece
		plus Sound end_piece
		Concatenate
		Rename... interval_'n'
		select Sound beg_piece
		plus Sound middle_piece
		plus Sound end_piece
		Remove

	elif label$ = "" and prevlabel$ = "a" and nextlabel$ = ""
		select LongSound 'file$'
		Extract part... beg beg+2*margin yes
		Rename... delete_1
		Extract part... beg beg+2*margin parabolic 1 yes
		Rename... delete_2
		Extract part... beg beg+margin rectangular 1 yes
		Rename... beg_piece
		select LongSound 'file$'
		Extract part... beg+margin end yes
		Rename... middle_piece
		select Sound delete_1
		plus Sound delete_2
		Remove
		select Sound beg_piece
		plus Sound middle_piece
		Concatenate
		Rename... interval_'n'
		select Sound beg_piece
		plus Sound middle_piece
		Remove

	elif label$ = "" and prevlabel$ = "" and nextlabel$ = "a"
		select LongSound 'file$'
		Extract part... beg end-margin yes
		Rename... middle_piece
		select LongSound 'file$'
		Extract part... end-2*margin end yes
		Rename... delete_3
		Extract part... end-2*margin end parabolic 1 yes
		Rename... delete_4
		Extract part... end-margin end rectangular 1 yes
		Rename... end_piece
		select Sound delete_3
		plus Sound delete_4
		Remove
		select Sound middle_piece
		plus Sound end_piece
		Concatenate
		Rename... interval_'n'
		select Sound middle_piece
		plus Sound end_piece
		Remove

	elif label$ = "" and prevlabel$ = "" and nextlabel$ = ""
		select LongSound 'file$'
		Extract part... beg end yes
		Rename... interval_'n'

	elif label$ <> ""
		echo Interval starting at 'beg:1' sec has label "'label$'". Labels must be either "a" or blank. See Pause form.
		beginPause ("Found an ambiguous interval label")
			comment ("What should be done with the interval labeled <'label$'> starting at 'beg:1' sec?")
			clicked = endPause ("Stop and fix label", "Anonymize it", 1)		
		if clicked = 1
			exit
		elif clicked = 2
			Set interval text... 1 n a
			goto RESTART
			endif
		endif

	echo Now saving interval 'n' of 'intervals'...

	select Sound interval_'n'
	if n = 1
		nowarn Save as WAV file... 'directory$''file$'_anonym.wav
	else
		nowarn Append to existing sound file... 'directory$''file$'_anonym.wav
		endif
	Remove
	beg = end
	endfor

echo Done!

