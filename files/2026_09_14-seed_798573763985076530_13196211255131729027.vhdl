-- Seed: 798573763985076530,13196211255131729027

entity rjdij is
  port (vguis : in integer; qlotjyqks : inout integer);
end rjdij;

architecture mcnjyigd of rjdij is
  
begin
  -- Single-driven assignments
  qlotjyqks <= vguis;
end mcnjyigd;

entity lks is
  port (lxg : in boolean_vector(1 to 0); udgcllj : buffer bit; ukglmwjyl : inout bit_vector(2 downto 4); grpgnisxer : in time);
end lks;

architecture wzqwwt of lks is
  signal g : integer;
  signal unc : integer;
  signal pa : integer;
begin
  vvfj : entity work.rjdij
    port map (vguis => pa, qlotjyqks => unc);
  glltyduvc : entity work.rjdij
    port map (vguis => g, qlotjyqks => pa);
  fbvzu : entity work.rjdij
    port map (vguis => pa, qlotjyqks => g);
end wzqwwt;

library ieee;
use ieee.std_logic_1164.all;

entity wysdwyd is
  port (y : in boolean; rhqnjqcxfc : in real; fao : buffer std_logic; vsewbf : in std_logic);
end wysdwyd;

architecture btcaqsobil of wysdwyd is
  signal tmg : integer;
begin
  goop : entity work.rjdij
    port map (vguis => tmg, qlotjyqks => tmg);
  
  -- Multi-driven assignments
  fao <= '0';
  fao <= vsewbf;
end btcaqsobil;

library ieee;
use ieee.std_logic_1164.all;

entity iihb is
  port (ubpwzw : linkage std_logic);
end iihb;

library ieee;
use ieee.std_logic_1164.all;

architecture gtxjekgg of iihb is
  signal uxjlmzifr : std_logic;
  signal ssgmwjk : std_logic;
  signal zbokqytjd : real;
  signal lsbs : boolean;
  signal anmtg : time;
  signal ln : bit_vector(2 downto 4);
  signal kwlj : bit;
  signal vsmdhayr : bit_vector(2 downto 4);
  signal erialp : bit;
  signal ntczbvhl : time;
  signal eumjrmu : bit_vector(2 downto 4);
  signal kvprhv : bit;
  signal u : boolean_vector(1 to 0);
begin
  jjzi : entity work.lks
    port map (lxg => u, udgcllj => kvprhv, ukglmwjyl => eumjrmu, grpgnisxer => ntczbvhl);
  e : entity work.lks
    port map (lxg => u, udgcllj => erialp, ukglmwjyl => vsmdhayr, grpgnisxer => ntczbvhl);
  tm : entity work.lks
    port map (lxg => u, udgcllj => kwlj, ukglmwjyl => ln, grpgnisxer => anmtg);
  nuo : entity work.wysdwyd
    port map (y => lsbs, rhqnjqcxfc => zbokqytjd, fao => ssgmwjk, vsewbf => uxjlmzifr);
  
  -- Single-driven assignments
  anmtg <= ntczbvhl;
  lsbs <= lsbs;
  ntczbvhl <= ntczbvhl;
  u <= u;
  
  -- Multi-driven assignments
  ssgmwjk <= ssgmwjk;
end gtxjekgg;



-- Seed after: 12950674918809363958,13196211255131729027
