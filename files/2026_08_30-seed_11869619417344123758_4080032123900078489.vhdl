-- Seed: 11869619417344123758,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity ady is
  port (qdofykqdqi : inout real; pvzhaacv : buffer std_logic; lkpnhs : out integer; ossy : linkage std_logic_vector(3 to 0));
end ady;

architecture cvomr of ady is
  
begin
  -- Single-driven assignments
  qdofykqdqi <= qdofykqdqi;
  lkpnhs <= 3;
end cvomr;

entity rtqcj is
  port (av : buffer bit; tcdfbvm : inout integer; kspnbjxir : inout integer);
end rtqcj;

library ieee;
use ieee.std_logic_1164.all;

architecture qbkcapc of rtqcj is
  signal lewlwqo : real;
  signal ovexlq : integer;
  signal ca : std_logic;
  signal ey : real;
  signal gguc : real;
  signal lnmfptapk : std_logic_vector(3 to 0);
  signal idoqzqezxy : integer;
  signal sx : std_logic;
  signal uaqm : real;
begin
  rnqhpioclf : entity work.ady
    port map (qdofykqdqi => uaqm, pvzhaacv => sx, lkpnhs => idoqzqezxy, ossy => lnmfptapk);
  brcokt : entity work.ady
    port map (qdofykqdqi => gguc, pvzhaacv => sx, lkpnhs => kspnbjxir, ossy => lnmfptapk);
  yuow : entity work.ady
    port map (qdofykqdqi => ey, pvzhaacv => ca, lkpnhs => ovexlq, ossy => lnmfptapk);
  dvh : entity work.ady
    port map (qdofykqdqi => lewlwqo, pvzhaacv => sx, lkpnhs => tcdfbvm, ossy => lnmfptapk);
  
  -- Single-driven assignments
  av <= '0';
  
  -- Multi-driven assignments
  sx <= 'Z';
  ca <= ca;
  sx <= 'L';
  ca <= '1';
end qbkcapc;



-- Seed after: 11365112843808131798,4080032123900078489
