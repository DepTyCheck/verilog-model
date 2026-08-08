-- Seed: 301039058723148297,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity xaf is
  port (zhh : inout time; v : in real; d : linkage time_vector(4 downto 0); nfkmti : inout std_logic_vector(2 downto 2));
end xaf;

architecture cbku of xaf is
  
begin
  
end cbku;

library ieee;
use ieee.std_logic_1164.all;

entity miylqc is
  port (hzlwgyqp : linkage std_logic_vector(1 downto 0); i : out std_logic; v : linkage integer);
end miylqc;

library ieee;
use ieee.std_logic_1164.all;

architecture awnrn of miylqc is
  signal vywjye : std_logic_vector(2 downto 2);
  signal kzsrwxcia : time_vector(4 downto 0);
  signal rksh : real;
  signal ytgzmxq : time;
  signal zmgrot : std_logic_vector(2 downto 2);
  signal jfroynki : time_vector(4 downto 0);
  signal wyuolevr : real;
  signal syq : time;
begin
  orbdvgrrbs : entity work.xaf
    port map (zhh => syq, v => wyuolevr, d => jfroynki, nfkmti => zmgrot);
  qnvgol : entity work.xaf
    port map (zhh => ytgzmxq, v => rksh, d => kzsrwxcia, nfkmti => vywjye);
  
  -- Single-driven assignments
  rksh <= 0_0_2_1.20240;
  wyuolevr <= wyuolevr;
  
  -- Multi-driven assignments
  i <= i;
  i <= '0';
  zmgrot <= zmgrot;
end awnrn;

entity okwevi is
  port (j : buffer string(4 downto 4));
end okwevi;

library ieee;
use ieee.std_logic_1164.all;

architecture gyuzgr of okwevi is
  signal baogsw : integer;
  signal znufkkeq : std_logic;
  signal ghiivtsuu : integer;
  signal eyfew : std_logic;
  signal mylehgn : std_logic_vector(1 downto 0);
  signal zvb : time_vector(4 downto 0);
  signal rkv : real;
  signal sivsrmqsqq : time;
  signal heod : std_logic_vector(2 downto 2);
  signal uyen : time_vector(4 downto 0);
  signal qhzhkg : real;
  signal znvbhbz : time;
begin
  ylqefyf : entity work.xaf
    port map (zhh => znvbhbz, v => qhzhkg, d => uyen, nfkmti => heod);
  bavwqnpxn : entity work.xaf
    port map (zhh => sivsrmqsqq, v => rkv, d => zvb, nfkmti => heod);
  kdtztrdkkl : entity work.miylqc
    port map (hzlwgyqp => mylehgn, i => eyfew, v => ghiivtsuu);
  tuieoqo : entity work.miylqc
    port map (hzlwgyqp => mylehgn, i => znufkkeq, v => baogsw);
  
  -- Single-driven assignments
  j <= j;
  qhzhkg <= 2#0.111#;
  
  -- Multi-driven assignments
  znufkkeq <= 'U';
  heod <= heod;
  heod <= (others => 'L');
  znufkkeq <= eyfew;
end gyuzgr;



-- Seed after: 9660776702182919471,8927267689619684183
