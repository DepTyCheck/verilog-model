-- Seed: 3324764145821842226,14629254427735353553

entity lfi is
  port (k : in integer_vector(3 downto 2));
end lfi;

architecture e of lfi is
  
begin
  
end e;

entity mkgydlg is
  port (d : inout integer; jh : linkage time; mkvmzipf : out integer);
end mkgydlg;

architecture jekbtj of mkgydlg is
  signal rvbpvmwne : integer_vector(3 downto 2);
begin
  qqtszhlhm : entity work.lfi
    port map (k => rvbpvmwne);
  stjhkunh : entity work.lfi
    port map (k => rvbpvmwne);
  irbhjrmzke : entity work.lfi
    port map (k => rvbpvmwne);
  
  -- Single-driven assignments
  rvbpvmwne <= (0, 2#0_1_1_1#);
  d <= 8#3_6_6_2_4#;
  mkvmzipf <= 8#124#;
end jekbtj;

library ieee;
use ieee.std_logic_1164.all;

entity pacspjfzly is
  port (tgwbhgp : out boolean; xntjlsaog : out std_logic_vector(0 to 3); iyikbibkw : out real);
end pacspjfzly;

architecture jik of pacspjfzly is
  signal gnjptq : integer_vector(3 downto 2);
begin
  vfvxhd : entity work.lfi
    port map (k => gnjptq);
  
  -- Single-driven assignments
  iyikbibkw <= 8#5_2_5_5_5.5_3_2_6#;
  gnjptq <= (1133, 8#12#);
  tgwbhgp <= TRUE;
  
  -- Multi-driven assignments
  xntjlsaog <= ('0', '0', 'L', 'U');
end jik;



-- Seed after: 7937712727694201104,14629254427735353553
