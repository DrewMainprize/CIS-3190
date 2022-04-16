--CIS*3190 - Assignment 2: Ada Image Processing
--University of Guelph
--For, Professor Michael Wirth
--
--
--Drew Mainprize
--1045298
with ada.text_IO; use ada.text_IO;
with ada.strings.unbounded; use ada.strings.unbounded;
with ada.strings.unbounded.text_IO; use ada.strings.unbounded.text_IO;
with ada.integer_text_IO; use ada.integer_text_IO;
with ada.strings.fixed; use ada.strings.fixed;
with imgrecord; use imgrecord;

package body imagepgm is

    infp : file_type;
    outfp : file_type;
    myImg : img;
    trimStr : unbounded_string;
    count : integer := 0;

    function readPGM(fileName : in unbounded_string) return img is
    begin

        open(infp, in_file, ada.strings.unbounded.to_string(fileName));

        --read in format and check for validity
        ada.strings.unbounded.text_IO.get_line(infp, myImg.format);

        if to_string(myImg.format) /= "P2" then
            put_line("Invalid Format - must be P2");
            close(infp);
            return myImg;
        end if;

        --read our initial param values, before the array
        ada.integer_text_IO.get(infp,myImg.width);
        ada.integer_text_IO.get(infp, myImg.height);
        ada.integer_text_IO.get(infp, myImg.maxVal);

        for i in 1..myImg.height loop
            for j in 1..myImg.width loop
                if count = myImg.height * myImg.width then
                    put_line("ERROR: File length and width/height do not match, retry");
                    close(infp);
                    return myImg;
                end if;

                ada.integer_text_IO.get(infp, myImg.pixels(i, j));
                count := count + 1;
            end loop;
        end loop;

        close(infp);

        return myImg;

    end readPGM;


    procedure writePGM(fileName : in unbounded_string; myImg : in img) is
    begin

        create(outfp, out_file, ada.strings.unbounded.to_string(fileName));

        put_line(outfp, myImg.format);
        ada.integer_text_IO.put(outfp, myImg.width, 0);
        put(outfp, " ");
        ada.integer_text_IO.put(outfp, myImg.height, 0);
        put_line(outfp, "");
        ada.integer_text_IO.put(outfp, myImg.maxVal, 0);
        put_line(outfp, "");

        for i in 1..myImg.height loop
            for j in 1..myImg.width loop
                trimStr := trim(to_unbounded_string(Integer'Image(myImg.pixels(i, j))), ada.strings.left);
                put(outfp, trimStr);
                put(outfp, " ");
            end loop;
            put_line(outfp, "");
        end loop;


        if is_open(outfp) then
            close(outfp);
        end if;


    end writePGM;

end imagepgm;
