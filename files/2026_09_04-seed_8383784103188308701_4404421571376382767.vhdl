-- Seed: 8383784103188308701,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity eydkixen is
  port ( wntwk : linkage std_logic_vector(4 downto 1)
  ; fyprlwvyk : linkage std_logic_vector(4 downto 1)
  ; nrxpsqk : inout time
  ; gaejilwf : buffer std_logic
  );
end eydkixen;

architecture h of eydkixen is
  
begin
  -- Single-driven assignments
  nrxpsqk <= nrxpsqk;
end h;

library ieee;
use ieee.std_logic_1164.all;

entity htjnksga is
  port (lspssqwliz : out std_logic);
end htjnksga;

library ieee;
use ieee.std_logic_1164.all;

architecture zrlanhlm of htjnksga is
  signal j : std_logic;
  signal gspl : time;
  signal mhnvzafde : time;
  signal neld : time;
  signal ggi : std_logic;
  signal iqfusuqhy : time;
  signal fzlbqec : std_logic_vector(4 downto 1);
  signal w : std_logic_vector(4 downto 1);
begin
  lzglalcwb : entity work.eydkixen
    port map (wntwk => w, fyprlwvyk => fzlbqec, nrxpsqk => iqfusuqhy, gaejilwf => ggi);
  slbiqwrbt : entity work.eydkixen
    port map (wntwk => fzlbqec, fyprlwvyk => w, nrxpsqk => neld, gaejilwf => lspssqwliz);
  htfopgzzo : entity work.eydkixen
    port map (wntwk => w, fyprlwvyk => w, nrxpsqk => mhnvzafde, gaejilwf => ggi);
  nbvs : entity work.eydkixen
    port map (wntwk => w, fyprlwvyk => w, nrxpsqk => gspl, gaejilwf => j);
  
  -- Multi-driven assignments
  w <= "ZU-L";
  j <= lspssqwliz;
  w <= "0X0-";
end zrlanhlm;



-- Seed after: 9482805402593519272,4404421571376382767
