-- Seed: 11404085302652941074,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity oveddibaqr is
  port (d : out std_logic_vector(3 downto 0); mcgivva : inout real_vector(4 to 3));
end oveddibaqr;

architecture tkqmwg of oveddibaqr is
  
begin
  -- Single-driven assignments
  mcgivva <= (others => 0.0);
  
  -- Multi-driven assignments
  d <= "Z0U-";
end tkqmwg;

entity aeau is
  port (ywfmoehl : linkage integer);
end aeau;

library ieee;
use ieee.std_logic_1164.all;

architecture qorfpvsg of aeau is
  signal idziqrmpr : real_vector(4 to 3);
  signal wqxy : std_logic_vector(3 downto 0);
  signal tjtb : real_vector(4 to 3);
  signal cvhi : std_logic_vector(3 downto 0);
  signal yqiyfx : real_vector(4 to 3);
  signal wps : std_logic_vector(3 downto 0);
begin
  zmkc : entity work.oveddibaqr
    port map (d => wps, mcgivva => yqiyfx);
  mudmb : entity work.oveddibaqr
    port map (d => cvhi, mcgivva => tjtb);
  uaf : entity work.oveddibaqr
    port map (d => wqxy, mcgivva => idziqrmpr);
  
  -- Multi-driven assignments
  cvhi <= ('1', '1', '0', 'Z');
  cvhi <= wps;
end qorfpvsg;



-- Seed after: 13056027310237025952,511364357853360275
