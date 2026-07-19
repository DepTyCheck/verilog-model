-- Seed: 10075994298480158420,5511103086789671269

entity tnr is
  port (vlzy : inout time_vector(2 to 4));
end tnr;

architecture puj of tnr is
  
begin
  -- Single-driven assignments
  vlzy <= (1330.24104 ps, 2 sec, 16#A7A8E.8564A# ms);
end puj;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (floxa : inout integer; qx : buffer time; giketskill : in std_logic_vector(0 downto 2); lcmnfsf : buffer character);
end f;

architecture yttebpdquh of f is
  signal xqqmpy : time_vector(2 to 4);
  signal kwu : time_vector(2 to 4);
  signal lbwaces : time_vector(2 to 4);
  signal fdwziw : time_vector(2 to 4);
begin
  uyjyqgte : entity work.tnr
    port map (vlzy => fdwziw);
  rydt : entity work.tnr
    port map (vlzy => lbwaces);
  bvfwa : entity work.tnr
    port map (vlzy => kwu);
  gakze : entity work.tnr
    port map (vlzy => xqqmpy);
  
  -- Single-driven assignments
  lcmnfsf <= lcmnfsf;
end yttebpdquh;



-- Seed after: 9313248023898281943,5511103086789671269
