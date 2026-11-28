-- Seed: 4472822315832318635,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity tupfqxvltr is
  port (tdf : inout real; mhintp : out std_logic_vector(3 downto 3));
end tupfqxvltr;

architecture km of tupfqxvltr is
  
begin
  -- Single-driven assignments
  tdf <= 16#A.23#;
  
  -- Multi-driven assignments
  mhintp <= (others => 'U');
  mhintp <= mhintp;
end km;

entity j is
  port (sopqbyvrut : in character; hxltioaj : linkage boolean; x : inout severity_level; dytsqqowga : out string(3 to 4));
end j;

library ieee;
use ieee.std_logic_1164.all;

architecture fapopr of j is
  signal pshouw : real;
  signal anpzhjkhp : real;
  signal xgwt : std_logic_vector(3 downto 3);
  signal sj : real;
begin
  hlbxa : entity work.tupfqxvltr
    port map (tdf => sj, mhintp => xgwt);
  xvzoky : entity work.tupfqxvltr
    port map (tdf => anpzhjkhp, mhintp => xgwt);
  jwzqawklrw : entity work.tupfqxvltr
    port map (tdf => pshouw, mhintp => xgwt);
  
  -- Single-driven assignments
  dytsqqowga <= ('d', 't');
  x <= ERROR;
end fapopr;



-- Seed after: 11822047169059072986,15306146742274979323
