-- Seed: 778651693989974070,8412319452373742525

library ieee;
use ieee.std_logic_1164.all;

entity unqqy is
  port (tlucnrcngm : out time_vector(0 downto 3); vu : buffer boolean; xhffgngr : linkage integer_vector(4 downto 0); hrc : in std_logic);
end unqqy;

architecture arvgpinui of unqqy is
  
begin
  
end arvgpinui;

library ieee;
use ieee.std_logic_1164.all;

entity mvji is
  port (tuhgwebian : inout std_logic_vector(4 to 3); nhcogjwx : buffer std_logic_vector(1 to 1));
end mvji;

library ieee;
use ieee.std_logic_1164.all;

architecture cqjthmqfa of mvji is
  signal ldbxz : std_logic;
  signal cmw : integer_vector(4 downto 0);
  signal ibyhscv : boolean;
  signal gabjjwyuuh : time_vector(0 downto 3);
  signal plgbbqusp : integer_vector(4 downto 0);
  signal hvluk : boolean;
  signal pllqo : time_vector(0 downto 3);
  signal pmhbiez : integer_vector(4 downto 0);
  signal vwh : boolean;
  signal px : time_vector(0 downto 3);
  signal yibrd : std_logic;
  signal vujpqh : integer_vector(4 downto 0);
  signal ppc : boolean;
  signal nikwbuf : time_vector(0 downto 3);
begin
  dygdxg : entity work.unqqy
    port map (tlucnrcngm => nikwbuf, vu => ppc, xhffgngr => vujpqh, hrc => yibrd);
  udzipmu : entity work.unqqy
    port map (tlucnrcngm => px, vu => vwh, xhffgngr => pmhbiez, hrc => yibrd);
  psftw : entity work.unqqy
    port map (tlucnrcngm => pllqo, vu => hvluk, xhffgngr => plgbbqusp, hrc => yibrd);
  tm : entity work.unqqy
    port map (tlucnrcngm => gabjjwyuuh, vu => ibyhscv, xhffgngr => cmw, hrc => ldbxz);
  
  -- Multi-driven assignments
  ldbxz <= 'Z';
  ldbxz <= 'L';
  nhcogjwx <= "X";
  ldbxz <= yibrd;
end cqjthmqfa;



-- Seed after: 10514687990123385750,8412319452373742525
