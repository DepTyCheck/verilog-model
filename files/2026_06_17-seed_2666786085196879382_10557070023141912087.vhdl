-- Seed: 2666786085196879382,10557070023141912087

library ieee;
use ieee.std_logic_1164.all;

entity jqpjhq is
  port (etztpcbz : in time; iitswqoh : buffer std_logic_vector(0 to 2); v : out time);
end jqpjhq;

architecture rwakqozn of jqpjhq is
  
begin
  -- Single-driven assignments
  v <= 00131.1_0_3_4_4 ps;
  
  -- Multi-driven assignments
  iitswqoh <= ('-', '-', 'X');
  iitswqoh <= ('-', 'W', 'Z');
end rwakqozn;

library ieee;
use ieee.std_logic_1164.all;

entity gxbbh is
  port (kunejyhtu : inout std_logic; wnjcrcn : buffer time; yvw : buffer std_logic);
end gxbbh;

library ieee;
use ieee.std_logic_1164.all;

architecture rqjikaj of gxbbh is
  signal itnzigkn : time;
  signal ajprj : time;
  signal lohafvzip : std_logic_vector(0 to 2);
  signal qqvatcvdle : time;
  signal fpgbbhlkc : time;
  signal rcjk : std_logic_vector(0 to 2);
begin
  grzhhezrmx : entity work.jqpjhq
    port map (etztpcbz => wnjcrcn, iitswqoh => rcjk, v => fpgbbhlkc);
  zsj : entity work.jqpjhq
    port map (etztpcbz => qqvatcvdle, iitswqoh => rcjk, v => wnjcrcn);
  bvxay : entity work.jqpjhq
    port map (etztpcbz => qqvatcvdle, iitswqoh => lohafvzip, v => ajprj);
  fibdoaxqv : entity work.jqpjhq
    port map (etztpcbz => itnzigkn, iitswqoh => rcjk, v => qqvatcvdle);
  
  -- Single-driven assignments
  itnzigkn <= 1_1_0.110 us;
  
  -- Multi-driven assignments
  yvw <= 'X';
  yvw <= '0';
end rqjikaj;



-- Seed after: 869695279515834237,10557070023141912087
