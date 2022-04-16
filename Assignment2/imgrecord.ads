with ada.strings.unbounded; use ada.strings.unbounded;

package imgrecord is
    
    type pixArr is array(integer range <>, integer range <>) of integer;
    type img is
        record
            format : unbounded_string;
            width : integer;
            height : integer;
            maxVal : integer;
            pixels : pixArr(1..500, 1..500);
        end record;

end imgrecord;