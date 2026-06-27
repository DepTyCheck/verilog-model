-- Seed: 11345884693730502822,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity kmchca is
  port (xc : out bit_vector(0 downto 3); hjqyq : out real; odn : out std_logic_vector(0 downto 0); gnoqsz : inout time);
end kmchca;

architecture haqmfmbfn of kmchca is
  
begin
  -- Single-driven assignments
  hjqyq <= 1_4_3_2.1_1_3;
  xc <= (others => '0');
  gnoqsz <= 8#66422.6_0_6# us;
  
  -- Multi-driven assignments
  odn <= (others => '-');
end haqmfmbfn;

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (lkupgu : buffer time; qkvuocxjz : inout std_logic; mykk : buffer integer_vector(4 to 1));
end s;

library ieee;
use ieee.std_logic_1164.all;

architecture gr of s is
  signal rkt : time;
  signal npfor : std_logic_vector(0 downto 0);
  signal unmnvcind : real;
  signal hqj : bit_vector(0 downto 3);
begin
  gtxxztp : entity work.kmchca
    port map (xc => hqj, hjqyq => unmnvcind, odn => npfor, gnoqsz => rkt);
  
  -- Single-driven assignments
  mykk <= (others => 0);
  lkupgu <= 2#0_0_1# us;
  
  -- Multi-driven assignments
  npfor <= "L";
  qkvuocxjz <= 'W';
end gr;

library ieee;
use ieee.std_logic_1164.all;

entity bwxwzw is
  port (kyzapdj : in std_logic_vector(3 to 4));
end bwxwzw;

library ieee;
use ieee.std_logic_1164.all;

architecture xf of bwxwzw is
  signal wwotedotm : time;
  signal we : real;
  signal ar : bit_vector(0 downto 3);
  signal jhbpbwsbow : time;
  signal ugalgh : std_logic_vector(0 downto 0);
  signal mqwxp : real;
  signal nomxtmoh : bit_vector(0 downto 3);
  signal exjfaxe : time;
  signal wjpph : std_logic_vector(0 downto 0);
  signal pzsbomimh : real;
  signal wv : bit_vector(0 downto 3);
  signal llbqtdt : integer_vector(4 to 1);
  signal uvcrorkxin : std_logic;
  signal gevludpofp : time;
begin
  luphweviz : entity work.s
    port map (lkupgu => gevludpofp, qkvuocxjz => uvcrorkxin, mykk => llbqtdt);
  xw : entity work.kmchca
    port map (xc => wv, hjqyq => pzsbomimh, odn => wjpph, gnoqsz => exjfaxe);
  akkatrogac : entity work.kmchca
    port map (xc => nomxtmoh, hjqyq => mqwxp, odn => ugalgh, gnoqsz => jhbpbwsbow);
  ctihanlrae : entity work.kmchca
    port map (xc => ar, hjqyq => we, odn => ugalgh, gnoqsz => wwotedotm);
  
  -- Multi-driven assignments
  uvcrorkxin <= 'L';
  uvcrorkxin <= '1';
  ugalgh <= "W";
  uvcrorkxin <= '1';
end xf;



-- Seed after: 15774066140544829436,4860866131898729603
