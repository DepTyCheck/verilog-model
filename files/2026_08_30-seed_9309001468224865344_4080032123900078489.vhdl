-- Seed: 9309001468224865344,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity cyotjtkrsl is
  port (nbvgutovd : out integer; ymikrkd : buffer std_logic_vector(4 to 2));
end cyotjtkrsl;

architecture lwxabmkc of cyotjtkrsl is
  
begin
  -- Single-driven assignments
  nbvgutovd <= 122;
end lwxabmkc;

library ieee;
use ieee.std_logic_1164.all;

entity nea is
  port (qqysiaap : inout std_logic; msu : inout integer; wdn : inout time);
end nea;

library ieee;
use ieee.std_logic_1164.all;

architecture bgpvzhs of nea is
  signal uimu : integer;
  signal odrs : integer;
  signal gliauw : std_logic_vector(4 to 2);
  signal v : integer;
  signal dptsduyw : std_logic_vector(4 to 2);
begin
  wfvswa : entity work.cyotjtkrsl
    port map (nbvgutovd => msu, ymikrkd => dptsduyw);
  gsqkf : entity work.cyotjtkrsl
    port map (nbvgutovd => v, ymikrkd => gliauw);
  xipbxt : entity work.cyotjtkrsl
    port map (nbvgutovd => odrs, ymikrkd => dptsduyw);
  phvxl : entity work.cyotjtkrsl
    port map (nbvgutovd => uimu, ymikrkd => dptsduyw);
  
  -- Single-driven assignments
  wdn <= 3_2.00030 us;
  
  -- Multi-driven assignments
  dptsduyw <= dptsduyw;
end bgpvzhs;

library ieee;
use ieee.std_logic_1164.all;

entity hlza is
  port (yzukxw : buffer std_logic; udmvxqcp : buffer std_logic; xzbp : buffer real_vector(3 downto 2); jhjamt : buffer time);
end hlza;

library ieee;
use ieee.std_logic_1164.all;

architecture ajmevz of hlza is
  signal pqfz : integer;
  signal s : time;
  signal bljv : integer;
  signal ohxx : std_logic;
  signal dblazk : integer;
  signal ratiila : std_logic;
  signal hxdf : std_logic_vector(4 to 2);
  signal clghlys : integer;
begin
  kekmc : entity work.cyotjtkrsl
    port map (nbvgutovd => clghlys, ymikrkd => hxdf);
  wbe : entity work.nea
    port map (qqysiaap => ratiila, msu => dblazk, wdn => jhjamt);
  ejk : entity work.nea
    port map (qqysiaap => ohxx, msu => bljv, wdn => s);
  fqbkzbtxzr : entity work.cyotjtkrsl
    port map (nbvgutovd => pqfz, ymikrkd => hxdf);
  
  -- Single-driven assignments
  xzbp <= xzbp;
  
  -- Multi-driven assignments
  udmvxqcp <= udmvxqcp;
  udmvxqcp <= udmvxqcp;
  udmvxqcp <= 'Z';
end ajmevz;



-- Seed after: 15697225256954891240,4080032123900078489
