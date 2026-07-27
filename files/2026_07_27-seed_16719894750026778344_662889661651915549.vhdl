-- Seed: 16719894750026778344,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity jrk is
  port (crwku : in std_logic_vector(3 downto 2); adhdziyenu : buffer boolean_vector(1 downto 0); etvvcjw : out time; opb : linkage real);
end jrk;

architecture nddjxwlo of jrk is
  
begin
  
end nddjxwlo;

library ieee;
use ieee.std_logic_1164.all;

entity nodfnogsj is
  port (ng : buffer time; uiw : inout std_logic);
end nodfnogsj;

architecture wjuoo of nodfnogsj is
  
begin
  -- Single-driven assignments
  ng <= ng;
  
  -- Multi-driven assignments
  uiw <= uiw;
  uiw <= '1';
  uiw <= uiw;
end wjuoo;

entity pumivabew is
  port (mu : linkage real);
end pumivabew;

library ieee;
use ieee.std_logic_1164.all;

architecture porgsc of pumivabew is
  signal kdurtqphm : time;
  signal xw : boolean_vector(1 downto 0);
  signal iew : std_logic_vector(3 downto 2);
  signal uolvsj : real;
  signal zqibf : time;
  signal j : boolean_vector(1 downto 0);
  signal sdzd : real;
  signal eegwgi : time;
  signal qlokowvnau : boolean_vector(1 downto 0);
  signal wdso : std_logic_vector(3 downto 2);
begin
  roza : entity work.jrk
    port map (crwku => wdso, adhdziyenu => qlokowvnau, etvvcjw => eegwgi, opb => sdzd);
  smeevk : entity work.jrk
    port map (crwku => wdso, adhdziyenu => j, etvvcjw => zqibf, opb => uolvsj);
  lcpmxble : entity work.jrk
    port map (crwku => iew, adhdziyenu => xw, etvvcjw => kdurtqphm, opb => mu);
  
  -- Multi-driven assignments
  wdso <= wdso;
  iew <= wdso;
  wdso <= wdso;
  wdso <= ('X', '1');
end porgsc;



-- Seed after: 5767921568208423532,662889661651915549
