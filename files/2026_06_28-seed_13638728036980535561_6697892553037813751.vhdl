-- Seed: 13638728036980535561,6697892553037813751

entity badeadayj is
  port (y : linkage time);
end badeadayj;

architecture ozj of badeadayj is
  
begin
  
end ozj;

entity fi is
  port (eygwqvjygg : out bit);
end fi;

architecture zoeoinxho of fi is
  signal aaqy : time;
  signal xzagenm : time;
  signal kncgmbcnwl : time;
  signal hllixzzo : time;
begin
  sveeley : entity work.badeadayj
    port map (y => hllixzzo);
  lm : entity work.badeadayj
    port map (y => kncgmbcnwl);
  kygip : entity work.badeadayj
    port map (y => xzagenm);
  jdxaqfrx : entity work.badeadayj
    port map (y => aaqy);
  
  -- Single-driven assignments
  eygwqvjygg <= '0';
end zoeoinxho;

library ieee;
use ieee.std_logic_1164.all;

entity ccxg is
  port (tsbyxy : inout std_logic; r : linkage boolean_vector(1 to 2); jdtdjx : out integer; afddpymqs : buffer severity_level);
end ccxg;

architecture uek of ccxg is
  signal bsage : time;
  signal zb : bit;
  signal tnzlr : time;
  signal zsk : bit;
begin
  atzu : entity work.fi
    port map (eygwqvjygg => zsk);
  b : entity work.badeadayj
    port map (y => tnzlr);
  nbupkh : entity work.fi
    port map (eygwqvjygg => zb);
  oxkfrz : entity work.badeadayj
    port map (y => bsage);
  
  -- Single-driven assignments
  jdtdjx <= 2#011#;
  afddpymqs <= WARNING;
  
  -- Multi-driven assignments
  tsbyxy <= 'L';
  tsbyxy <= 'X';
end uek;

library ieee;
use ieee.std_logic_1164.all;

entity lx is
  port (b : out std_logic; tpb : buffer boolean; mdwyps : buffer std_logic_vector(3 to 2); l : out std_logic_vector(0 downto 1));
end lx;

library ieee;
use ieee.std_logic_1164.all;

architecture cttvnkqkn of lx is
  signal tc : severity_level;
  signal zdkdssa : integer;
  signal e : boolean_vector(1 to 2);
  signal sfkdafcl : std_logic;
begin
  qpaz : entity work.ccxg
    port map (tsbyxy => sfkdafcl, r => e, jdtdjx => zdkdssa, afddpymqs => tc);
  
  -- Single-driven assignments
  tpb <= TRUE;
  
  -- Multi-driven assignments
  sfkdafcl <= '1';
  mdwyps <= "";
  l <= "";
end cttvnkqkn;



-- Seed after: 9207934636087609487,6697892553037813751
