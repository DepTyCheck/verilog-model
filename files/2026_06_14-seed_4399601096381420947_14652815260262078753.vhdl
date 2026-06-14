-- Seed: 4399601096381420947,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity kzwsut is
  port (aw : linkage std_logic_vector(3 downto 0); wuwlfzl : in boolean; fzyq : linkage real; blvqqu : out time_vector(3 to 2));
end kzwsut;

architecture p of kzwsut is
  
begin
  -- Single-driven assignments
  blvqqu <= (others => 0 ns);
end p;

library ieee;
use ieee.std_logic_1164.all;

entity ct is
  port (gf : out std_logic; drzjbsgj : in std_logic_vector(4 downto 4));
end ct;

library ieee;
use ieee.std_logic_1164.all;

architecture o of ct is
  signal siehjixzzb : time_vector(3 to 2);
  signal mitanm : real;
  signal gyyfveb : boolean;
  signal kaselumn : std_logic_vector(3 downto 0);
begin
  twkhfbyu : entity work.kzwsut
    port map (aw => kaselumn, wuwlfzl => gyyfveb, fzyq => mitanm, blvqqu => siehjixzzb);
  
  -- Single-driven assignments
  gyyfveb <= TRUE;
  
  -- Multi-driven assignments
  gf <= 'Z';
  kaselumn <= ('Z', 'U', '0', 'X');
end o;

entity gqxpvgi is
  port (oimzsmkzr : out time);
end gqxpvgi;

library ieee;
use ieee.std_logic_1164.all;

architecture cg of gqxpvgi is
  signal lwdtgg : time_vector(3 to 2);
  signal lyyawai : real;
  signal pkwm : time_vector(3 to 2);
  signal ovbbw : real;
  signal amnmhhwno : boolean;
  signal ip : std_logic_vector(3 downto 0);
begin
  vhtlbecp : entity work.kzwsut
    port map (aw => ip, wuwlfzl => amnmhhwno, fzyq => ovbbw, blvqqu => pkwm);
  ulw : entity work.kzwsut
    port map (aw => ip, wuwlfzl => amnmhhwno, fzyq => lyyawai, blvqqu => lwdtgg);
  
  -- Multi-driven assignments
  ip <= ('X', 'H', 'H', 'L');
end cg;

entity m is
  port (hozc : buffer time);
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture kfboxlldc of m is
  signal ubl : std_logic_vector(4 downto 4);
  signal wrcypvdl : std_logic;
  signal haro : time;
begin
  kble : entity work.gqxpvgi
    port map (oimzsmkzr => haro);
  jpvvf : entity work.ct
    port map (gf => wrcypvdl, drzjbsgj => ubl);
  
  -- Single-driven assignments
  hozc <= 33432.1_4_3 ms;
end kfboxlldc;



-- Seed after: 12242690745742072324,14652815260262078753
