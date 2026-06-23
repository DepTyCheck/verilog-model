-- Seed: 2539039489898048391,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity xyohdc is
  port (bp : in time; fifad : inout std_logic; zzn : buffer time);
end xyohdc;

architecture t of xyohdc is
  
begin
  -- Multi-driven assignments
  fifad <= '0';
  fifad <= 'X';
  fifad <= '-';
end t;

entity miqeqekmoy is
  port (fkyfp : in character; fscn : inout boolean_vector(0 to 4));
end miqeqekmoy;

library ieee;
use ieee.std_logic_1164.all;

architecture cohw of miqeqekmoy is
  signal csitnfhxj : time;
  signal bomhmvn : std_logic;
  signal td : time;
  signal a : std_logic;
  signal aftzs : time;
begin
  insv : entity work.xyohdc
    port map (bp => aftzs, fifad => a, zzn => aftzs);
  yeym : entity work.xyohdc
    port map (bp => td, fifad => bomhmvn, zzn => csitnfhxj);
  
  -- Single-driven assignments
  fscn <= (TRUE, FALSE, FALSE, FALSE, TRUE);
  td <= 2#0.1# ms;
  
  -- Multi-driven assignments
  a <= 'L';
end cohw;

entity lzuk is
  port (eiklxskd : out string(2 to 5));
end lzuk;

architecture bhknkxebn of lzuk is
  signal vhvdhiguq : boolean_vector(0 to 4);
  signal qwekfgexte : character;
begin
  eimkhgpcww : entity work.miqeqekmoy
    port map (fkyfp => qwekfgexte, fscn => vhvdhiguq);
  
  -- Single-driven assignments
  eiklxskd <= "qvcz";
end bhknkxebn;



-- Seed after: 5062392968154522189,8421704836678237495
