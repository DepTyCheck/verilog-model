-- Seed: 7726006201609076605,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity jyulflbdlf is
  port (ohu : out std_logic; eg : in std_logic_vector(1 to 2); dbqov : inout std_logic);
end jyulflbdlf;

architecture qpdmbmcbgt of jyulflbdlf is
  
begin
  -- Multi-driven assignments
  dbqov <= '0';
end qpdmbmcbgt;

library ieee;
use ieee.std_logic_1164.all;

entity cnhouw is
  port (v : in integer; za : in real_vector(3 to 4); abz : linkage std_logic_vector(1 to 2); hvt : buffer time);
end cnhouw;

architecture cgqykyyup of cnhouw is
  
begin
  -- Single-driven assignments
  hvt <= 2#000.1111# fs;
end cgqykyyup;

entity peitiprnxt is
  port (qzenl : inout time; oebwis : linkage severity_level);
end peitiprnxt;

architecture wx of peitiprnxt is
  
begin
  -- Single-driven assignments
  qzenl <= 1014.2 ns;
end wx;

entity sxncspssod is
  port (fxx : linkage bit; nsmobfzgp : out bit);
end sxncspssod;

library ieee;
use ieee.std_logic_1164.all;

architecture gui of sxncspssod is
  signal kgozvrfmc : std_logic_vector(1 to 2);
  signal aylorfakqc : std_logic;
  signal npn : severity_level;
  signal tvmwa : time;
begin
  i : entity work.peitiprnxt
    port map (qzenl => tvmwa, oebwis => npn);
  phlrw : entity work.jyulflbdlf
    port map (ohu => aylorfakqc, eg => kgozvrfmc, dbqov => aylorfakqc);
  
  -- Single-driven assignments
  nsmobfzgp <= '0';
  
  -- Multi-driven assignments
  aylorfakqc <= '1';
  aylorfakqc <= 'H';
end gui;



-- Seed after: 13512399383053840705,6882842853887419669
