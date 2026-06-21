-- Seed: 6911710837787673350,3687118713772291287

entity xvrqvb is
  port (tkjgjntid : out bit_vector(0 to 4));
end xvrqvb;

architecture vcouzaxfg of xvrqvb is
  
begin
  -- Single-driven assignments
  tkjgjntid <= ('1', '1', '0', '0', '1');
end vcouzaxfg;

entity pfgysiid is
  port (izucdyk : buffer string(1 downto 2));
end pfgysiid;

architecture zkqxkmfvr of pfgysiid is
  signal nbgrv : bit_vector(0 to 4);
begin
  gcvtweoosx : entity work.xvrqvb
    port map (tkjgjntid => nbgrv);
  
  -- Single-driven assignments
  izucdyk <= "";
end zkqxkmfvr;

library ieee;
use ieee.std_logic_1164.all;

entity zpgzjnwfe is
  port (ldxsmzwtte : in bit; vtyp : linkage std_logic);
end zpgzjnwfe;

architecture asokantg of zpgzjnwfe is
  signal qbkij : bit_vector(0 to 4);
  signal v : bit_vector(0 to 4);
  signal cae : string(1 downto 2);
  signal codpfnclvz : bit_vector(0 to 4);
begin
  koeir : entity work.xvrqvb
    port map (tkjgjntid => codpfnclvz);
  qbimseg : entity work.pfgysiid
    port map (izucdyk => cae);
  qnp : entity work.xvrqvb
    port map (tkjgjntid => v);
  x : entity work.xvrqvb
    port map (tkjgjntid => qbkij);
end asokantg;



-- Seed after: 1522318605210487303,3687118713772291287
