-- Seed: 10860959731635838041,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity dsnvypb is
  port (xawahfojuv : buffer std_logic_vector(4 to 4); zlrm : inout time_vector(4 to 3));
end dsnvypb;

architecture uis of dsnvypb is
  
begin
  -- Multi-driven assignments
  xawahfojuv <= xawahfojuv;
  xawahfojuv <= xawahfojuv;
end uis;

library ieee;
use ieee.std_logic_1164.all;

entity ujfaw is
  port (k : in std_logic; yqjshmgco : in bit_vector(1 downto 4); vcwjue : out integer);
end ujfaw;

library ieee;
use ieee.std_logic_1164.all;

architecture dc of ujfaw is
  signal mygcndjvmr : time_vector(4 to 3);
  signal ffdjqa : time_vector(4 to 3);
  signal zqhxydjthk : time_vector(4 to 3);
  signal nepiidnslk : std_logic_vector(4 to 4);
begin
  mssxcpm : entity work.dsnvypb
    port map (xawahfojuv => nepiidnslk, zlrm => zqhxydjthk);
  bwn : entity work.dsnvypb
    port map (xawahfojuv => nepiidnslk, zlrm => ffdjqa);
  fuvnw : entity work.dsnvypb
    port map (xawahfojuv => nepiidnslk, zlrm => mygcndjvmr);
  
  -- Single-driven assignments
  vcwjue <= 1;
  
  -- Multi-driven assignments
  nepiidnslk <= nepiidnslk;
  nepiidnslk <= (others => 'H');
  nepiidnslk <= nepiidnslk;
end dc;



-- Seed after: 2267087624227571295,4404421571376382767
