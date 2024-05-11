with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Characters.Handling;
with Ada.numerics.discrete_random;
with Ada.strings.fixed;


use Ada.Strings.Unbounded, Ada.Strings.Unbounded.Text_IO;
use Ada.Text_IO;
use Ada.strings.fixed;

--program starts here
procedure Hangman is 
	--string subtype declaration for use in functions
	subtype string20 is string(1..20);
	
	--standard string to unbounded_string converter
	function tub(Source : String) return unbounded_string renames ada.strings.unbounded.to_unbounded_string;
	
	--pads out unbounded strings 
	function padString(V: unbounded_string) return string20 is
		temp : constant string := to_string(V);
		res : string20;
	begin
		ada.strings.fixed.move(temp, res, drop=>ada.strings.right);
		return res;
	end padString;
	
	--draws the hangman
	function draw(markDown: integer)return integer is
		drawPicture: array(1..12,1..12) of character;
		I: integer;
		J: integer;
	begin
		for I in 1..12 loop
			for J in 1..12 loop
				drawPicture(I,J):=' ';
			end loop;
		end loop;
		I:=1;
		for I in 1..12 loop
			drawPicture(I,1):='X';
		end loop;
		J:= 1;
		for J in 1..7 loop
			drawPicture(1,J):='X';
		end loop;
	
	drawPicture(2,7):='X';
		
		if markDown = 1 then
			put_line("First we draw a head.");
		end if;
		if markDown = 2 then
			put_line("Now we draw answer body.");
		end if;
		if markDown = 3 then
			put_line("Next we draw an arm.");
		end if;
		if markDown = 4 then
			put_line("This time it's the other arm.");
		end if;
		if markDown = 5 then
			put_line("Now, let's draw the right leg.");
		end if;
		if markDown = 6 then
			put_line("This time we draw the left leg.");
		end if;
		if markDown = 7 then
			put_line("Now we put up answer hand.");
		end if;
		if markDown = 8 then
			put_line("Next the other hand.");
		end if;
		if markDown = 9 then
			put_line("Now we draw one foot.");
		end if;
		if markDown = 10 then
			put_line("Here's the other foot -- You're hung!!.");
		end if;
		
		if markDown >= 1 then
			drawPicture(3,6):= '-';
			drawPicture(3,7):= '-';
			drawPicture(3,8):= '-';
			drawPicture(4,5):= '('; 
			drawPicture(4,6):= '.'; 
			drawPicture(4,8):= '.';
			drawPicture(4,9):= ')';
			drawPicture(5,6):= '-';
			drawPicture(5,7):= '-'; 
			drawPicture(5,8):= '-';
		end if;
		if markDown >= 2 then
			for I in 6..9 loop
				drawPicture(I,7):= 'X';
			end loop;
		end if;
		if markDown >= 3 then
			for I in 4..7 loop
				drawPicture(I,I-1):= '\';
			end loop;
		end if;
		if markDown >= 4 then
			drawPicture(4,11):= '/';
			drawPicture(5,10):= '/';
			drawPicture(6,9):= '/';
			drawPicture(7,8):= '/';
		end if;
		if markDown >= 5 then
			drawPicture(10,6):= '/';
			drawPicture(11,5):= '/';
		end if;
		if markDown >= 6 then
			drawPicture(10,8):= '\';
			drawPicture(11,9):= '\';
		end if;
		if markDown >= 7 then
			drawPicture(3,11):= '\';
		end if;
		if markDown >= 8 then
			drawPicture(3,3):= '/';
		end if;
		if markDown >= 9 then
			drawPicture(12,10):= '\';
			drawPicture(12,11):= '-';
		end if;
		if markDown = 10 then
			drawPicture(12,3):= '-';
			drawPicture(12,4):= '/';
		end if;
		
		for I in 1..12 loop
			for J in 1..12 loop
				put(drawPicture(I,J));
			end loop;
			new_line;
		end loop;
		return markDown;
	end draw;
	
	--for the dictionary
	type dictionary is array (1..50) of unbounded_string;
	Dict: dictionary;
	
	--for the randomizer
	type randRange is new Integer range 1..50;
	package Rand_Int is new ada.numerics.discrete_random(randRange);
	package IntIO is new Integer_IO(Integer);
	use Rand_Int;
	gen: Generator;
	
	D: array(1..20) of character;
	N: array(1..26) of character;
	U: array(1..50) of integer;
	
	answer: string20; --the answer
	bestGuess: string(1..20); --best guess
	
	guess: character; --guessing letter
	yesOrNo: character; --ask if you want to play again letter

	Q: integer;
	markDown: integer; --marks down th number of errors.
	I: integer;  --iterative integer
	J: integer;  --iterative integer
	W: integer;
	T1: integer; --number of guesses
	R: integer;
	wordLength: integer;  --word length
	C: integer;
	
begin
	put_line("THE GAME OF HANGMAN");
	<<ten>>
	--setting up the dictionary
	Dict:= (tub("gum"),tub("sin"),tub("for"),tub("cry"),tub("lug"),tub("bye"),tub("fly"),tub("ugly"),tub("each"),tub("from"),tub("work"),tub("talk"),tub("with"),tub("self"),tub("pizza"),tub("thing"),tub("feign"),tub("fiend"),tub("elbow"),tub("fault"),tub("dirty"),tub("budget"),tub("spirit"),tub("quaint"),tub("maiden"),tub("escort"),tub("pickax"),tub("example"),tub("tension"),tub("quinine"),tub("kidney"),tub("replica"),tub("sleeper"),tub("triangle"),tub("kangaroo"),tub("mahogany"),tub("sergeant"),tub("sequence"),tub("moustache"),tub("dangerous"),tub("scientist"),tub("different"),tub("quiescent"),tub("magistrate"),tub("erroneously"),tub("loudspeaker"),tub("phytotoxic"),tub("matrimonial"),tub("parasympathomimetic"),tub("thigmotropism"));
	
	--setup string outputs
	I:= 1;
	for I in 1..20 loop
		D(I):='-';
	end loop;
	I:= 1;
	for I in 1..26 loop
		N(I):=' ';
	end loop;
	I:= 1;
	for I in 1..50 loop
		U(I):=0;
	end loop;
	
    C:= 1;
    W:= 50;
    markDown:= 0;
	
	if C < W then
		<<oneHundred>>
		reset(gen);
		Q:= integer(random(gen));
		
		if U(Q) - 1 = 0 then
			goto oneHundred; --ensures an index that isn't zero
		else
			U(Q):= 1;
			C:= C + 1;
			T1:= 0;
			
			answer:= padString(Dict(Q));
			wordLength:= length(Dict(Q));

			I:= 1;
			for I in 1..wordLength loop
				put(D(I));
			end loop;
			new_line;
			
			<<oneSeventy>>
			put_line("Here are the letters you used: "); --displays the number of letters used
			
			for I in 1..26 loop
				if N(I) = ' ' then
					goto twoHundred;
				end if;
				put(N(I) & ',');
			end loop;
			new_line;
			
			<<twoHundred>>
			put_line(" ");
			put_line("What is your guess? "); --asks for word prompt
			R:= 0;
			get(guess);
			skip_line;
			
			I:= 1;
			for I in 1..26 loop
				if N(I) = ' ' then
					goto twoFifty;
				end if;
				if N(I) = guess then
					put_line("You guessed that letter before"); --checks for guessed letter
					goto oneSeventy;
				end if;
			end loop;
			put_line("Invalid character"); --checks for non lower case alphabetical characters
			goto oneSeventy;
			<<twoFifty>>
			T1:= T1 + 1;
			N(T1):= guess;
			
			I:= 1;
			for I in 1..wordLength loop
				if answer(I) = guess then 
					D(I):= guess;
					R:= R + 1;
				end if;
			end loop;
			
			if R = 0 then
				markDown:= markDown + 1; --keeps track of wrong guesses.
			else
				I:= 1;
				for I in 1..wordLength loop
					if D(I) /= '-' then
						for I in 1..wordLength loop
							put(D(I));
						end loop;
						new_line;

						put_line("What is your guess for the word? "); --guess the word
						get_line(bestGuess, wordLength);
						
						J:= 1;
						for J in 1..wordLength loop
							if answer(J) /= bestGuess(J) then
								put_line("Wrong. Try another letter"); --response given on wrong letter
								goto oneSeventy;
							end if;
						end loop;
						
						put("Right! It took you "); --tells you the number of guesses it took to get the words right
						IntIO.put(T1);
						put(" guesses");
						new_line;

						put_line("Do you want another word? (Y/N) "); --ask if you want to play again
						get(yesOrNo);
						skip_line;
							
						if yesOrNo = 'Y' then
							goto ten;
						else
							put_line("It's been fun! Bye for now.");
							goto nineNinenine;
						end if;
					end if;
				end loop;
			end if;
			put_line("Sorry, that letter isn't in the word.");
			markDown:= draw(markDown); -- calls the draw function
			if markDown < 10 then
				goto oneSeventy;
			end if;
			
			put("Sorry, you lose. The word was "); --gives the answer on a loss
			I:= 1;
			for I in 1..20 loop
				put(answer(I));
			end loop;
			new_line;
			
			put_line("Do you want another word? (Y/N) "); --ask if you want to play again
			get(yesOrNo);
			skip_line;
							
			if yesOrNo = 'Y' then
				goto ten;
			end if;
			put_line("It's been fun! Bye for now.");
		end if;
	else
		put_line("You did all the words");
	end if;
	<<nineNinenine>>
end Hangman;

