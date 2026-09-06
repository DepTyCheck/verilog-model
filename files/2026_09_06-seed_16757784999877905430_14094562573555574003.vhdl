-- Seed: 16757784999877905430,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity phyngucpb is
  port (fclek : out std_logic; dxk : out boolean_vector(4 to 0); dln : inout bit_vector(4 to 0); logytqwkt : buffer bit);
end phyngucpb;

architecture x of phyngucpb is
  
begin
  -- Single-driven assignments
  dxk <= (others => TRUE);
  dln <= dln;
  logytqwkt <= '1';
  
  -- Multi-driven assignments
  fclek <= fclek;
  fclek <= 'L';
  fclek <= fclek;
end x;

library ieee;
use ieee.std_logic_1164.all;

entity bc is
  port (rbqsdkkvsd : in std_logic; juaswa : out real);
end bc;

library ieee;
use ieee.std_logic_1164.all;

architecture cn of bc is
  signal wx : bit;
  signal fdeylrchm : bit_vector(4 to 0);
  signal judb : boolean_vector(4 to 0);
  signal vhho : bit;
  signal qzcdxnpgp : bit_vector(4 to 0);
  signal ms : boolean_vector(4 to 0);
  signal nuafji : std_logic;
begin
  bmhgjnos : entity work.phyngucpb
    port map (fclek => nuafji, dxk => ms, dln => qzcdxnpgp, logytqwkt => vhho);
  jjr : entity work.phyngucpb
    port map (fclek => nuafji, dxk => judb, dln => fdeylrchm, logytqwkt => wx);
  
  -- Single-driven assignments
  juaswa <= 1.0_3_2;
  
  -- Multi-driven assignments
  nuafji <= rbqsdkkvsd;
  nuafji <= 'L';
end cn;



-- Seed after: 16430185424133181853,14094562573555574003
