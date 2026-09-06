-- Seed: 6155831590901426763,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity agchs is
  port ( ijcdby : in bit_vector(1 to 4)
  ; kkntuvfllc : buffer std_logic_vector(2 to 1)
  ; bezmddl : out std_logic_vector(4 downto 0)
  ; fvrvxc : buffer character
  );
end agchs;

architecture phcipel of agchs is
  
begin
  -- Single-driven assignments
  fvrvxc <= fvrvxc;
end phcipel;

library ieee;
use ieee.std_logic_1164.all;

entity zyvputdp is
  port (kvqkvgz : out std_logic; kup : buffer time_vector(2 to 4));
end zyvputdp;

library ieee;
use ieee.std_logic_1164.all;

architecture tbelgksiaa of zyvputdp is
  signal ccvghkxny : character;
  signal rco : std_logic_vector(4 downto 0);
  signal k : std_logic_vector(2 to 1);
  signal r : bit_vector(1 to 4);
begin
  qbdbgyc : entity work.agchs
    port map (ijcdby => r, kkntuvfllc => k, bezmddl => rco, fvrvxc => ccvghkxny);
  
  -- Single-driven assignments
  kup <= kup;
  r <= ('1', '1', '0', '1');
  
  -- Multi-driven assignments
  rco <= ('U', 'H', 'L', '-', '1');
end tbelgksiaa;

entity cpgzvczh is
  port (t : out character);
end cpgzvczh;

library ieee;
use ieee.std_logic_1164.all;

architecture jnnhcrh of cpgzvczh is
  signal zk : character;
  signal noor : bit_vector(1 to 4);
  signal nzgj : time_vector(2 to 4);
  signal neljuyjrib : std_logic;
  signal s : character;
  signal ir : std_logic_vector(4 downto 0);
  signal csnvw : std_logic_vector(2 to 1);
  signal dzfmufgjk : bit_vector(1 to 4);
begin
  n : entity work.agchs
    port map (ijcdby => dzfmufgjk, kkntuvfllc => csnvw, bezmddl => ir, fvrvxc => s);
  oxapadetvs : entity work.zyvputdp
    port map (kvqkvgz => neljuyjrib, kup => nzgj);
  qbxad : entity work.agchs
    port map (ijcdby => noor, kkntuvfllc => csnvw, bezmddl => ir, fvrvxc => zk);
  
  -- Single-driven assignments
  t <= 'r';
  noor <= dzfmufgjk;
  dzfmufgjk <= dzfmufgjk;
  
  -- Multi-driven assignments
  csnvw <= csnvw;
  csnvw <= csnvw;
  neljuyjrib <= neljuyjrib;
  neljuyjrib <= 'W';
end jnnhcrh;



-- Seed after: 199676123740078266,14094562573555574003
