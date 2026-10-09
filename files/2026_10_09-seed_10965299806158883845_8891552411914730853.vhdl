-- Seed: 10965299806158883845,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity btifoccqo is
  port (ysqxlc : buffer std_logic_vector(2 downto 0); kx : linkage real; vhvcv : buffer time; ifcx : in bit);
end btifoccqo;

architecture frprvirwx of btifoccqo is
  
begin
  -- Single-driven assignments
  vhvcv <= 0 min;
  
  -- Multi-driven assignments
  ysqxlc <= ysqxlc;
  ysqxlc <= ('H', '-', 'L');
  ysqxlc <= ('W', 'X', 'U');
  ysqxlc <= ysqxlc;
end frprvirwx;

entity zqlg is
  port (lyw : out time; eljtakqf : linkage severity_level; e : buffer integer; vl : buffer bit_vector(0 to 4));
end zqlg;

library ieee;
use ieee.std_logic_1164.all;

architecture p of zqlg is
  signal fnupulahjf : real;
  signal lkrw : std_logic_vector(2 downto 0);
  signal hftlhska : bit;
  signal qxlkloepv : time;
  signal umboaiw : real;
  signal ul : std_logic_vector(2 downto 0);
  signal cqtt : bit;
  signal qmyvc : time;
  signal odaptym : real;
  signal wzfnov : std_logic_vector(2 downto 0);
begin
  ptaslwlus : entity work.btifoccqo
    port map (ysqxlc => wzfnov, kx => odaptym, vhvcv => qmyvc, ifcx => cqtt);
  otkiuhomhl : entity work.btifoccqo
    port map (ysqxlc => ul, kx => umboaiw, vhvcv => qxlkloepv, ifcx => hftlhska);
  exhsnr : entity work.btifoccqo
    port map (ysqxlc => lkrw, kx => fnupulahjf, vhvcv => lyw, ifcx => cqtt);
  
  -- Single-driven assignments
  hftlhska <= '0';
  vl <= ('1', '0', '1', '1', '0');
  
  -- Multi-driven assignments
  ul <= wzfnov;
  wzfnov <= ('X', 'U', 'W');
end p;



-- Seed after: 17820724829896052036,8891552411914730853
