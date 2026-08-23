-- Seed: 7940251604111958138,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port ( hujhkwp : in time
  ; qcvj : linkage std_logic_vector(3 to 4)
  ; rrzj : linkage std_logic_vector(4 downto 3)
  ; ogsquho : buffer std_logic_vector(4 downto 4)
  );
end q;

architecture ecp of q is
  
begin
  -- Multi-driven assignments
  ogsquho <= (others => 'L');
  ogsquho <= ogsquho;
end ecp;

library ieee;
use ieee.std_logic_1164.all;

entity nk is
  port ( gadc : in real_vector(2 to 0)
  ; psm : in std_logic_vector(0 downto 3)
  ; ukjmtftoq : linkage std_logic_vector(0 to 4)
  ; uwmhvjrz : in std_logic_vector(2 to 1)
  );
end nk;

library ieee;
use ieee.std_logic_1164.all;

architecture ccnkzsvx of nk is
  signal yoxikqn : std_logic_vector(4 downto 3);
  signal gd : std_logic_vector(3 to 4);
  signal hldrnrlego : std_logic_vector(4 downto 4);
  signal ljaq : std_logic_vector(4 downto 3);
  signal qqzpcfpnp : std_logic_vector(4 downto 3);
  signal rok : time;
begin
  lwdojyn : entity work.q
    port map (hujhkwp => rok, qcvj => qqzpcfpnp, rrzj => ljaq, ogsquho => hldrnrlego);
  psmlkasyhd : entity work.q
    port map (hujhkwp => rok, qcvj => qqzpcfpnp, rrzj => qqzpcfpnp, ogsquho => hldrnrlego);
  ivcf : entity work.q
    port map (hujhkwp => rok, qcvj => gd, rrzj => yoxikqn, ogsquho => hldrnrlego);
  
  -- Single-driven assignments
  rok <= rok;
end ccnkzsvx;



-- Seed after: 17374857113485549836,4245627776430562977
