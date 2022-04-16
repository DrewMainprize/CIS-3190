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
with ada.numerics.elementary_functions; use ada.numerics.elementary_functions;
with imgrecord; use imgrecord;


package body imageprocess is

    hist : histArr;
    ch : histArr;
    finalHist : finalArr;
    numPixels : float;
    histImg : img;

    procedure imageINV(myImg : in out img) is
    begin

        for i in 1..myImg.height loop
            for j in 1..myImg.width loop
                myImg.pixels(i, j) := abs (myImg.maxVal - myImg.pixels(i, j));
            end loop;
        end loop;

    end imageINV;

    procedure imageLOG(myImg : in out img) is
    begin

        for i in 1..myImg.height loop
            for j in 1..myImg.width loop
                myImg.pixels(i, j) := Integer(log(Float(myImg.pixels(i, j))) * (float(myImg.maxVal) / log(float(myImg.maxVal))));
            end loop;
        end loop;

    end imageLOG;

    procedure imageSTRETCH(myImg : in out img; iMin : in integer; iMax : in integer) is
    begin

        for i in 1..myImg.height loop
            for j in 1..myImg.width loop
                myImg.pixels(i, j) := Integer(   float(myImg.maxVal) *   ((float(myImg.pixels(i, j)) - float(iMin)) / (float(iMax) - float(iMin)))     );  
            end loop;
        end loop;

    end imageSTRETCH;

    function makeHIST(myImg : in img) return finalArr is

    begin

        for i in 1..256 loop
            hist(i) := 0.0;
        end loop;

        --1. Calculate the histogram of the grayscale image.
        for i in 1..myImg.height loop
            for j in 1..myImg.width loop
                --Calc histogram of greyscale image
                hist(myImg.pixels(i,j) + 1) := hist(myImg.pixels(i, j) + 1) + 1.0;
            end loop;
        end loop;

        numPixels := float(myImg.height * myImg.width);

        --2. Calculate the probability density function (PDF) 
        for i in 1..256 loop
            hist(i) := hist(i) / numPixels;
        end loop;

        --3. Calculate the cumulative histogram (CH).
        for i in 1..256 loop
            if i = 1 then
                ch(i) := hist(i);
            else
                ch(i) := hist(i) + ch(i - 1);
            end if;
        end loop;

        --4. Multiply the CH by 255 and round
        for i in 1..256 loop
            finalHist(i) := integer(ch(i) * 255.0);
        end loop;

        return finalHist;

    end makeHIST;

    function histEQUAL(myImg : in img; theHist : finalArr) return img is
    begin
        --We need to return an image for this func, so assign all vals of myImg to histImg
        histImg.format := myImg.format;
        histImg.width := myImg.width;
        histImg.height := myImg.height;
        histImg.maxVal := myImg.maxVal;

        --5. Map the new grayscale values from step 4
        for i in 1..histImg.height loop
            for j in 1..histImg.width loop
                histImg.pixels(i, j) := theHist(myImg.pixels(i, j) + 1);
            end loop;
        end loop;

        return histImg;

    end histEQUAL;

end imageprocess;
