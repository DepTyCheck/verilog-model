-- Seed: 1168897967463020611,12359743974512393525

library ieee;
use ieee.std_logic_1164.all;

entity gmrc is
  port (clpxa : inout real; irlb : inout real; bnobjdxl : linkage std_logic; dilr : in string(2 to 2));
end gmrc;

architecture hhhzdaxpr of gmrc is
  
begin
  -- Single-driven assignments
  irlb <= irlb;
  clpxa <= 16#A42E.2#;
end hhhzdaxpr;

entity mfpadvrh is
  port (ym : linkage integer; vcywjx : out time);
end mfpadvrh;

library ieee;
use ieee.std_logic_1164.all;

architecture ihf of mfpadvrh is
  signal timyddp : string(2 to 2);
  signal csd : std_logic;
  signal mkkhonup : real;
  signal jvptd : real;
  signal ekhaxjhje : string(2 to 2);
  signal mt : std_logic;
  signal hbkpwaz : real;
  signal bwunieknh : real;
  signal sty : string(2 to 2);
  signal eyf : std_logic;
  signal rlpfj : real;
  signal rpwymg : real;
begin
  gyphumm : entity work.gmrc
    port map (clpxa => rpwymg, irlb => rlpfj, bnobjdxl => eyf, dilr => sty);
  qaxsi : entity work.gmrc
    port map (clpxa => bwunieknh, irlb => hbkpwaz, bnobjdxl => mt, dilr => ekhaxjhje);
  oelboe : entity work.gmrc
    port map (clpxa => jvptd, irlb => mkkhonup, bnobjdxl => csd, dilr => timyddp);
  
  -- Multi-driven assignments
  mt <= eyf;
  mt <= 'L';
end ihf;

library ieee;
use ieee.std_logic_1164.all;

entity kw is
  port (xinmxkmprn : out std_logic_vector(0 downto 3); lhuq : out real);
end kw;

library ieee;
use ieee.std_logic_1164.all;

architecture buaawybtgr of kw is
  signal ljtmqdw : time;
  signal kmanysut : integer;
  signal mnvass : real;
  signal lpgita : real;
  signal hojsw : real;
  signal reou : string(2 to 2);
  signal mazqf : std_logic;
  signal hg : real;
  signal ygovfs : real;
begin
  cy : entity work.gmrc
    port map (clpxa => ygovfs, irlb => hg, bnobjdxl => mazqf, dilr => reou);
  exs : entity work.gmrc
    port map (clpxa => hojsw, irlb => lpgita, bnobjdxl => mazqf, dilr => reou);
  wkpzde : entity work.gmrc
    port map (clpxa => mnvass, irlb => lhuq, bnobjdxl => mazqf, dilr => reou);
  vycvrmv : entity work.mfpadvrh
    port map (ym => kmanysut, vcywjx => ljtmqdw);
  
  -- Single-driven assignments
  reou <= reou;
  
  -- Multi-driven assignments
  xinmxkmprn <= xinmxkmprn;
end buaawybtgr;



-- Seed after: 1151242386686664522,12359743974512393525
