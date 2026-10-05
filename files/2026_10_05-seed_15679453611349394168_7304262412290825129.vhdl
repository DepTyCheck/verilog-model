-- Seed: 15679453611349394168,7304262412290825129

entity x is
  port (gswfe : linkage integer_vector(0 to 1));
end x;

architecture kq of x is
  
begin
  
end kq;

library ieee;
use ieee.std_logic_1164.all;

entity unrx is
  port (ldjbeoyxt : out std_logic_vector(2 downto 3); vix : linkage integer; f : linkage integer; rhwlflp : buffer std_logic_vector(3 to 3));
end unrx;

architecture r of unrx is
  signal ldcfoiym : integer_vector(0 to 1);
  signal kveprofmad : integer_vector(0 to 1);
begin
  xsgzpndmu : entity work.x
    port map (gswfe => kveprofmad);
  adgbp : entity work.x
    port map (gswfe => ldcfoiym);
  
  -- Multi-driven assignments
  rhwlflp <= rhwlflp;
  rhwlflp <= rhwlflp;
  rhwlflp <= (others => '-');
end r;



-- Seed after: 5870977474726609914,7304262412290825129
