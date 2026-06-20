-- Seed: 4099502540490747766,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity jkxyxwgt is
  port (qwrbl : linkage std_logic);
end jkxyxwgt;

architecture mohp of jkxyxwgt is
  
begin
  
end mohp;

entity vfndxd is
  port (mqhqqssna : out bit_vector(3 downto 0));
end vfndxd;

library ieee;
use ieee.std_logic_1164.all;

architecture pw of vfndxd is
  signal cprbxmea : std_logic;
  signal dvybmgw : std_logic;
  signal evb : std_logic;
  signal qhxzayljh : std_logic;
begin
  rbvwvbcce : entity work.jkxyxwgt
    port map (qwrbl => qhxzayljh);
  kxf : entity work.jkxyxwgt
    port map (qwrbl => evb);
  tlscjh : entity work.jkxyxwgt
    port map (qwrbl => dvybmgw);
  mh : entity work.jkxyxwgt
    port map (qwrbl => cprbxmea);
  
  -- Single-driven assignments
  mqhqqssna <= ('0', '0', '0', '1');
  
  -- Multi-driven assignments
  qhxzayljh <= 'L';
end pw;

entity cssv is
  port (cijn : inout integer);
end cssv;

library ieee;
use ieee.std_logic_1164.all;

architecture dpcn of cssv is
  signal qinsqdakz : bit_vector(3 downto 0);
  signal d : std_logic;
  signal tlsxmvk : std_logic;
begin
  ieglgcncpf : entity work.jkxyxwgt
    port map (qwrbl => tlsxmvk);
  w : entity work.jkxyxwgt
    port map (qwrbl => d);
  poicbfynck : entity work.vfndxd
    port map (mqhqqssna => qinsqdakz);
  
  -- Single-driven assignments
  cijn <= 2#1_0#;
  
  -- Multi-driven assignments
  tlsxmvk <= '1';
end dpcn;



-- Seed after: 9456846814569228174,17924494779688682807
