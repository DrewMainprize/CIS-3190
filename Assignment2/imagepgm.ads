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
with imgrecord; use imgrecord;


package imagepgm is
    function readPGM(fileName : in unbounded_string) return img;
    procedure writePGM(fileName : in unbounded_string; myImg : in img);

end imagepgm;