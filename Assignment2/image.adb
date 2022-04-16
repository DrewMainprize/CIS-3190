--CIS*3190 - Assignment 2: Ada Image Processing
--University of Guelph
--For, Professor Michael Wirth
--
--
--Drew Mainprize
--1045298


with Ada.Text_IO; use Ada.Text_IO;
with ada.directories; use ada.directories;
with ada.strings.unbounded; use ada.strings.unbounded;
with ada.strings.unbounded.text_IO; use ada.strings.unbounded.text_IO;
with imagepgm; use imagepgm;
with imgrecord; use imgrecord;
with imageprocess; use imageprocess;


procedure image is
    choice : integer;
    choiceStr : unbounded_string;
    fn : unbounded_string;
    stopProg : boolean := false;
    iMin : integer;
    iMax : integer;
    iStr : unbounded_string;
    myHist : finalArr;

    newImg : img;


    function getFilename(io : character) return unbounded_string is
        fname : unbounded_string;
        nameExists : boolean := false; --var to check if filename exists
        overwrite : unbounded_string;
        goodName : boolean := false; -- var to check if can use filename for writing
    begin

        put_line("Enter filename: ");
        --this file input loop was retrieved from https://craftofcoding.wordpress.com/
        if io in 'r' then
            --loop until the file name exists
            loop
                exit when nameExists;
                get_line(fname);
                nameExists := exists(to_string(fname));
                --if it doesn't exist, print an error message before trying again
                if not nameExists then
                    put_line("ERROR: File does not exist, try again: ");
                end if;
            end loop;

        else
            loop
                exit when goodName;
                --if file exists, ask if we can overwrite, else just return
                get_line(fname);
                nameExists := exists(to_string(fname));

                --if exists, ask to overwrite
                if nameExists then
                    put_line("File exists, okay to overwrite? -> type yes or no");
                    get_line(overwrite);
                    --overStr := to_string(overwrite);

                    if to_string(overwrite) = "yes" then
                        goodName := true;
                    else
                        put_line("Enter Filename: ");
                    end if;
                else 
                    goodName := true;
                end if;
            end loop;

        end if;

        return fname;

    end getFilename;

begin
    --Main wrapper program

    loop
        exit when stopProg;
        put_line("Image Processing");
        put_line("1. Read in PGM image from file");
        put_line("2. Apply image invertion");
        put_line("3. Apply LOG function");
        put_line("4. Apply contrast stretching");
        put_line("5. Apply histogram equalization");
        put_line("6. Write PGM image to file");
        put_line("7. Quit");

        put_line("Option:");
        choice := -1;
        get_line(choiceStr);
        choice := Integer'Value(to_string(choiceStr));


        case choice is
            when 1 =>
                fn := getFilename('r');
                newImg := readPGM(fn);
            when 2 =>
                imageINV(newImg);
            when 3 =>
                imageLOG(newImg);
            when 4 =>
                put_line("Enter iMin value: ");
                get_line(iStr);
                iMin := Integer'Value(to_string(iStr));

                put_line("Enter iMax value: ");
                get_line(iStr);
                iMax := Integer'Value(to_string(iStr));

                imageSTRETCH(newImg, iMin, iMax);
            when 5 =>
                myHist := makeHIST(newImg);
                newImg := histEQUAL(newImg, myHist);
            when 6 =>
                fn := getFilename('w');
                writePGM(fn, newImg);
            when 7 =>
                stopProg := true;
            when others =>
                null;
        end case;
    end loop;

end image;
