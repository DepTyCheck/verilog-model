-- Seed: 13780627313655240690,4080032123900078489

entity sxktl is
  port (ybi : buffer bit_vector(0 to 3));
end sxktl;

architecture gmcnumauu of sxktl is
  
begin
  -- Single-driven assignments
  ybi <= ('1', '1', '0', '1');
end gmcnumauu;

entity k is
  port (l : in real_vector(2 downto 0); kfheaoiwhi : in real);
end k;

architecture qzg of k is
  signal hwgkm : bit_vector(0 to 3);
  signal jqxcnnoe : bit_vector(0 to 3);
begin
  etuxttajp : entity work.sxktl
    port map (ybi => jqxcnnoe);
  wdcfxiu : entity work.sxktl
    port map (ybi => hwgkm);
end qzg;

entity sngeotd is
  port (pew : linkage integer_vector(1 downto 1); ebhv : buffer integer; vw : linkage string(2 to 2));
end sngeotd;

architecture rrfoozem of sngeotd is
  signal bcijo : real_vector(2 downto 0);
  signal w : real;
  signal ckizjcq : real_vector(2 downto 0);
  signal h : bit_vector(0 to 3);
begin
  eccftpzmxk : entity work.sxktl
    port map (ybi => h);
  tzer : entity work.k
    port map (l => ckizjcq, kfheaoiwhi => w);
  yvpogxsdkt : entity work.k
    port map (l => bcijo, kfheaoiwhi => w);
end rrfoozem;

library ieee;
use ieee.std_logic_1164.all;

entity htccsmcir is
  port (cmuvfj : in integer; gtvhl : in real; ufbw : buffer real; y : linkage std_logic_vector(1 to 3));
end htccsmcir;

architecture z of htccsmcir is
  signal tcyk : bit_vector(0 to 3);
  signal xvbr : bit_vector(0 to 3);
begin
  vhrc : entity work.sxktl
    port map (ybi => xvbr);
  cbe : entity work.sxktl
    port map (ybi => tcyk);
end z;



-- Seed after: 11055145045033637923,4080032123900078489
