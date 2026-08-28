-- Seed: 16341318737984185694,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity ih is
  port (cuborqm : inout std_logic_vector(2 to 1));
end ih;

architecture thnetbsup of ih is
  
begin
  
end thnetbsup;

entity hoqgvqjlas is
  port (qt : buffer real);
end hoqgvqjlas;

library ieee;
use ieee.std_logic_1164.all;

architecture izltly of hoqgvqjlas is
  signal misemrvvk : std_logic_vector(2 to 1);
  signal uqdxkuhpn : std_logic_vector(2 to 1);
  signal svuq : std_logic_vector(2 to 1);
begin
  qlk : entity work.ih
    port map (cuborqm => svuq);
  qp : entity work.ih
    port map (cuborqm => uqdxkuhpn);
  ddedpxt : entity work.ih
    port map (cuborqm => misemrvvk);
  m : entity work.ih
    port map (cuborqm => svuq);
  
  -- Single-driven assignments
  qt <= 1204.211;
  
  -- Multi-driven assignments
  svuq <= (others => '0');
  svuq <= svuq;
  uqdxkuhpn <= "";
  svuq <= svuq;
end izltly;



-- Seed after: 6320911632676583448,7198033922882419595
