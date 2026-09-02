-- Seed: 1138418365828786203,3400751927341804175

entity matdarh is
  port (trju : buffer time);
end matdarh;

architecture b of matdarh is
  
begin
  -- Single-driven assignments
  trju <= 4 min;
end b;

library ieee;
use ieee.std_logic_1164.all;

entity jnzaeczpn is
  port (lmcbcykidw : linkage bit; wjqvie : in std_logic_vector(4 downto 0); uhf : out std_logic_vector(4 to 3); he : out time);
end jnzaeczpn;

architecture jszxilx of jnzaeczpn is
  
begin
  -- Single-driven assignments
  he <= he;
  
  -- Multi-driven assignments
  uhf <= (others => '0');
  uhf <= (others => '0');
end jszxilx;

entity yt is
  port (btcgz : buffer boolean; vwyh : in bit_vector(4 downto 0));
end yt;

architecture hxijaeu of yt is
  signal x : time;
  signal hb : time;
  signal gxzqg : time;
  signal qblyiid : time;
begin
  xci : entity work.matdarh
    port map (trju => qblyiid);
  pobyl : entity work.matdarh
    port map (trju => gxzqg);
  aqhkzqkuzk : entity work.matdarh
    port map (trju => hb);
  hapgnu : entity work.matdarh
    port map (trju => x);
  
  -- Single-driven assignments
  btcgz <= FALSE;
end hxijaeu;

library ieee;
use ieee.std_logic_1164.all;

entity oqgj is
  port ( dxrygirhzb : linkage real
  ; nzveyuaju : in integer_vector(3 downto 1)
  ; f : buffer std_logic_vector(1 downto 2)
  ; nyuxabpk : in time_vector(4 downto 1)
  );
end oqgj;

architecture atir of oqgj is
  signal pskhfght : time;
  signal oealbmztr : time;
  signal fj : bit_vector(4 downto 0);
  signal vnzihgk : boolean;
  signal zqs : bit_vector(4 downto 0);
  signal i : boolean;
begin
  uylkkts : entity work.yt
    port map (btcgz => i, vwyh => zqs);
  yfixxzbjr : entity work.yt
    port map (btcgz => vnzihgk, vwyh => fj);
  hshkprwoy : entity work.matdarh
    port map (trju => oealbmztr);
  iytxcqr : entity work.matdarh
    port map (trju => pskhfght);
  
  -- Single-driven assignments
  zqs <= ('1', '0', '1', '1', '0');
  fj <= zqs;
  
  -- Multi-driven assignments
  f <= (others => '0');
  f <= f;
  f <= f;
end atir;



-- Seed after: 18002651604594193625,3400751927341804175
