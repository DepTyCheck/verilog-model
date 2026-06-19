-- Seed: 7413264737910460033,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity nkbukm is
  port (dciwpyvdnp : linkage std_logic_vector(2 downto 3); efkfoq : in std_logic_vector(2 to 1));
end nkbukm;

architecture kwstxfvk of nkbukm is
  
begin
  
end kwstxfvk;

library ieee;
use ieee.std_logic_1164.all;

entity poekra is
  port (uivfyx : linkage std_logic; i : in integer_vector(3 downto 2); svlahnxzws : buffer severity_level; ttt : linkage std_logic_vector(0 downto 2));
end poekra;

library ieee;
use ieee.std_logic_1164.all;

architecture px of poekra is
  signal qjcvbdg : std_logic_vector(2 downto 3);
  signal ymqpxotg : std_logic_vector(2 to 1);
begin
  jedcqh : entity work.nkbukm
    port map (dciwpyvdnp => ymqpxotg, efkfoq => ymqpxotg);
  v : entity work.nkbukm
    port map (dciwpyvdnp => qjcvbdg, efkfoq => ymqpxotg);
  
  -- Single-driven assignments
  svlahnxzws <= ERROR;
  
  -- Multi-driven assignments
  ymqpxotg <= (others => '0');
  qjcvbdg <= (others => '0');
  ymqpxotg <= (others => '0');
  ymqpxotg <= (others => '0');
end px;

entity wqjacwhib is
  port (tqvxgraqm : buffer bit);
end wqjacwhib;

library ieee;
use ieee.std_logic_1164.all;

architecture lpiimevp of wqjacwhib is
  signal hoaxcawknw : severity_level;
  signal lgrt : integer_vector(3 downto 2);
  signal lpgo : std_logic;
  signal bouloknf : std_logic_vector(2 to 1);
  signal halqtark : std_logic_vector(0 downto 2);
begin
  ddfizrfvgh : entity work.nkbukm
    port map (dciwpyvdnp => halqtark, efkfoq => bouloknf);
  pcasovyo : entity work.poekra
    port map (uivfyx => lpgo, i => lgrt, svlahnxzws => hoaxcawknw, ttt => halqtark);
  
  -- Single-driven assignments
  tqvxgraqm <= '1';
  lgrt <= (16#70B79#, 3_0);
end lpiimevp;

entity w is
  port (kgklkaz : out time);
end w;

library ieee;
use ieee.std_logic_1164.all;

architecture lfga of w is
  signal nmvpddotk : bit;
  signal d : std_logic_vector(2 to 1);
  signal gcoeoswvte : std_logic_vector(2 downto 3);
begin
  vubhdq : entity work.nkbukm
    port map (dciwpyvdnp => gcoeoswvte, efkfoq => d);
  zrerhdqpz : entity work.nkbukm
    port map (dciwpyvdnp => gcoeoswvte, efkfoq => d);
  c : entity work.wqjacwhib
    port map (tqvxgraqm => nmvpddotk);
  
  -- Single-driven assignments
  kgklkaz <= 1 hr;
  
  -- Multi-driven assignments
  gcoeoswvte <= "";
  d <= (others => '0');
  gcoeoswvte <= "";
  gcoeoswvte <= "";
end lfga;



-- Seed after: 1849012171193483534,3108530264173481209
