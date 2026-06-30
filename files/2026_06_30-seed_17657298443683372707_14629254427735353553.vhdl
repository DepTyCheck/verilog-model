-- Seed: 17657298443683372707,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity ksngzjm is
  port (nrpejl : in std_logic_vector(0 to 0); cfhhee : inout boolean; tewnubn : out boolean_vector(2 downto 3));
end ksngzjm;

architecture plyybjvqtg of ksngzjm is
  
begin
  -- Single-driven assignments
  tewnubn <= (others => TRUE);
  cfhhee <= FALSE;
end plyybjvqtg;

entity b is
  port (hjxig : in bit; u : out time);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture trr of b is
  signal jngh : boolean_vector(2 downto 3);
  signal ubyqntgpf : boolean;
  signal yldxhd : std_logic_vector(0 to 0);
  signal xokwdmy : boolean_vector(2 downto 3);
  signal stlv : boolean;
  signal bwwnysct : std_logic_vector(0 to 0);
  signal t : boolean_vector(2 downto 3);
  signal gtkucgztdz : boolean;
  signal sdbzrqc : std_logic_vector(0 to 0);
begin
  ryaiy : entity work.ksngzjm
    port map (nrpejl => sdbzrqc, cfhhee => gtkucgztdz, tewnubn => t);
  by : entity work.ksngzjm
    port map (nrpejl => bwwnysct, cfhhee => stlv, tewnubn => xokwdmy);
  sudaqmktz : entity work.ksngzjm
    port map (nrpejl => yldxhd, cfhhee => ubyqntgpf, tewnubn => jngh);
  
  -- Single-driven assignments
  u <= 16#7_1# us;
  
  -- Multi-driven assignments
  sdbzrqc <= "W";
end trr;

library ieee;
use ieee.std_logic_1164.all;

entity mrahxshpu is
  port (oeiwieo : buffer std_logic_vector(1 downto 3));
end mrahxshpu;

library ieee;
use ieee.std_logic_1164.all;

architecture jktlsersz of mrahxshpu is
  signal vhkxit : time;
  signal aithrcb : bit;
  signal azlmaduemz : boolean_vector(2 downto 3);
  signal esblnz : boolean;
  signal pw : std_logic_vector(0 to 0);
  signal wnrnyabrq : boolean_vector(2 downto 3);
  signal n : boolean;
  signal ao : std_logic_vector(0 to 0);
begin
  jyumbkojq : entity work.ksngzjm
    port map (nrpejl => ao, cfhhee => n, tewnubn => wnrnyabrq);
  tejlh : entity work.ksngzjm
    port map (nrpejl => pw, cfhhee => esblnz, tewnubn => azlmaduemz);
  nqzbeea : entity work.b
    port map (hjxig => aithrcb, u => vhkxit);
  
  -- Single-driven assignments
  aithrcb <= '1';
  
  -- Multi-driven assignments
  oeiwieo <= "";
end jktlsersz;



-- Seed after: 11049910257238220570,14629254427735353553
