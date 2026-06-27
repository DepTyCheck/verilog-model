-- Seed: 17647704328480151553,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity gmvpx is
  port (qadjvpxikw : inout real; crfoxdf : out std_logic_vector(3 to 3); mcpidmtham : out integer; niwmxx : in std_logic);
end gmvpx;

architecture pee of gmvpx is
  
begin
  -- Single-driven assignments
  mcpidmtham <= 8#6_6_7_1_0#;
  
  -- Multi-driven assignments
  crfoxdf <= "-";
  crfoxdf <= (others => '1');
end pee;

entity zjcbmwng is
  port (bhj : inout severity_level);
end zjcbmwng;

library ieee;
use ieee.std_logic_1164.all;

architecture yon of zjcbmwng is
  signal gmxglkz : integer;
  signal ixmpxi : std_logic_vector(3 to 3);
  signal dodoq : real;
  signal yrmo : std_logic;
  signal chpqvwbv : integer;
  signal dvnnyykar : std_logic_vector(3 to 3);
  signal xoxtn : real;
  signal rz : std_logic;
  signal lrtrbpypmq : integer;
  signal bzyysnone : std_logic_vector(3 to 3);
  signal nd : real;
begin
  adfhedoy : entity work.gmvpx
    port map (qadjvpxikw => nd, crfoxdf => bzyysnone, mcpidmtham => lrtrbpypmq, niwmxx => rz);
  debwoczawl : entity work.gmvpx
    port map (qadjvpxikw => xoxtn, crfoxdf => dvnnyykar, mcpidmtham => chpqvwbv, niwmxx => yrmo);
  ajqpwnlk : entity work.gmvpx
    port map (qadjvpxikw => dodoq, crfoxdf => ixmpxi, mcpidmtham => gmxglkz, niwmxx => rz);
  
  -- Single-driven assignments
  bhj <= WARNING;
  
  -- Multi-driven assignments
  dvnnyykar <= (others => 'L');
  bzyysnone <= (others => 'L');
  bzyysnone <= (others => 'X');
  ixmpxi <= "-";
end yon;



-- Seed after: 2084726711873996496,4860866131898729603
