-- Seed: 12229538535371157163,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity jx is
  port (vutdd : in std_logic_vector(2 to 0));
end jx;

architecture afmzfhvq of jx is
  
begin
  
end afmzfhvq;

entity tlztd is
  port (gr : out integer; nbd : buffer string(2 to 2));
end tlztd;

library ieee;
use ieee.std_logic_1164.all;

architecture okizdprhor of tlztd is
  signal gkkloaa : std_logic_vector(2 to 0);
begin
  ksvaxbu : entity work.jx
    port map (vutdd => gkkloaa);
  gejm : entity work.jx
    port map (vutdd => gkkloaa);
  pus : entity work.jx
    port map (vutdd => gkkloaa);
  
  -- Single-driven assignments
  nbd <= "i";
  
  -- Multi-driven assignments
  gkkloaa <= "";
  gkkloaa <= gkkloaa;
  gkkloaa <= gkkloaa;
  gkkloaa <= "";
end okizdprhor;

entity mz is
  port (fvtpzmrv : inout severity_level; ox : in real; yw : buffer integer_vector(0 to 0); fyq : inout integer);
end mz;

library ieee;
use ieee.std_logic_1164.all;

architecture pdqdhamfe of mz is
  signal hxtirje : std_logic_vector(2 to 0);
  signal vyccrrfml : string(2 to 2);
  signal gzikyn : string(2 to 2);
  signal lovtlosvh : integer;
  signal w : string(2 to 2);
  signal egna : integer;
begin
  idqjmtlcpf : entity work.tlztd
    port map (gr => egna, nbd => w);
  dkmpuxpnom : entity work.tlztd
    port map (gr => lovtlosvh, nbd => gzikyn);
  lscar : entity work.tlztd
    port map (gr => fyq, nbd => vyccrrfml);
  yvmgcx : entity work.jx
    port map (vutdd => hxtirje);
  
  -- Single-driven assignments
  fvtpzmrv <= ERROR;
  yw <= (others => 3);
end pdqdhamfe;



-- Seed after: 6720610779143562742,10594830431004325987
