-- Seed: 1198079241123011888,4404421571376382767

entity uavsa is
  port (ptp : out integer_vector(4 downto 2); idlpfwdua : linkage time);
end uavsa;

architecture ehpsd of uavsa is
  
begin
  -- Single-driven assignments
  ptp <= (333, 8#10127#, 16#17266#);
end ehpsd;

entity x is
  port (mjmfq : inout real);
end x;

architecture c of x is
  
begin
  -- Single-driven assignments
  mjmfq <= 8#1623.675#;
end c;

library ieee;
use ieee.std_logic_1164.all;

entity vqmioz is
  port (louwza : in std_logic_vector(2 to 1); cyopfpb : buffer time);
end vqmioz;

architecture fnxclo of vqmioz is
  signal omkayh : real;
  signal kextuo : time;
  signal miy : integer_vector(4 downto 2);
  signal pgjqqvyvad : time;
  signal kc : integer_vector(4 downto 2);
  signal hzuszacg : integer_vector(4 downto 2);
begin
  dljcq : entity work.uavsa
    port map (ptp => hzuszacg, idlpfwdua => cyopfpb);
  jvfllx : entity work.uavsa
    port map (ptp => kc, idlpfwdua => pgjqqvyvad);
  urnpwzmqzv : entity work.uavsa
    port map (ptp => miy, idlpfwdua => kextuo);
  qblzvj : entity work.x
    port map (mjmfq => omkayh);
end fnxclo;



-- Seed after: 17069326760196939382,4404421571376382767
