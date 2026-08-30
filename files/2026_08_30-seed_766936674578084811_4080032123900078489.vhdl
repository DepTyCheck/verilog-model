-- Seed: 766936674578084811,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity mjlblmgqe is
  port (bazp : buffer std_logic; lrmtpnybiy : out time_vector(2 to 3));
end mjlblmgqe;

architecture s of mjlblmgqe is
  
begin
  -- Single-driven assignments
  lrmtpnybiy <= (241 ns, 4 us);
  
  -- Multi-driven assignments
  bazp <= bazp;
end s;

entity kn is
  port (onxd : buffer integer; ofrzxqamqk : buffer time);
end kn;

library ieee;
use ieee.std_logic_1164.all;

architecture siqw of kn is
  signal nxibvocad : time_vector(2 to 3);
  signal omdulqju : std_logic;
  signal oig : time_vector(2 to 3);
  signal eafyrg : time_vector(2 to 3);
  signal h : time_vector(2 to 3);
  signal qxavgkeakf : std_logic;
begin
  vcac : entity work.mjlblmgqe
    port map (bazp => qxavgkeakf, lrmtpnybiy => h);
  pprci : entity work.mjlblmgqe
    port map (bazp => qxavgkeakf, lrmtpnybiy => eafyrg);
  wq : entity work.mjlblmgqe
    port map (bazp => qxavgkeakf, lrmtpnybiy => oig);
  wnykosnql : entity work.mjlblmgqe
    port map (bazp => omdulqju, lrmtpnybiy => nxibvocad);
  
  -- Single-driven assignments
  ofrzxqamqk <= 0_1_4_3_4 ns;
  onxd <= 2_0_1_0;
  
  -- Multi-driven assignments
  omdulqju <= 'H';
end siqw;



-- Seed after: 1185460185540016540,4080032123900078489
