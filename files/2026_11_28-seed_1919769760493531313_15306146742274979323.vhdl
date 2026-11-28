-- Seed: 1919769760493531313,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity zstr is
  port (rntz : out std_logic_vector(1 downto 0));
end zstr;

architecture ivka of zstr is
  
begin
  -- Multi-driven assignments
  rntz <= ('W', 'L');
  rntz <= rntz;
end ivka;

entity d is
  port (qtdwlq : in boolean; dtclmv : inout real_vector(2 to 1); guqfuabw : out bit; qirobu : inout boolean);
end d;

architecture bdqc of d is
  
begin
  -- Single-driven assignments
  guqfuabw <= guqfuabw;
  dtclmv <= (others => 0.0);
  qirobu <= FALSE;
end bdqc;

entity bxymszvha is
  port (juqbmcxeug : buffer boolean_vector(0 to 2); pumhi : linkage integer; kzsglyzow : in real; req : out severity_level);
end bxymszvha;

library ieee;
use ieee.std_logic_1164.all;

architecture ptke of bxymszvha is
  signal wnzkld : boolean;
  signal epyl : bit;
  signal dppefhf : real_vector(2 to 1);
  signal ldhl : boolean;
  signal gfmvoacblo : std_logic_vector(1 downto 0);
begin
  uwfmfu : entity work.zstr
    port map (rntz => gfmvoacblo);
  i : entity work.d
    port map (qtdwlq => ldhl, dtclmv => dppefhf, guqfuabw => epyl, qirobu => wnzkld);
  ijyuo : entity work.zstr
    port map (rntz => gfmvoacblo);
  
  -- Single-driven assignments
  juqbmcxeug <= (FALSE, FALSE, FALSE);
  ldhl <= ldhl;
  req <= WARNING;
end ptke;

entity padunkddt is
  port (txglo : in integer; bajaws : buffer real; k : buffer time);
end padunkddt;

architecture ddinsn of padunkddt is
  signal ykpy : boolean;
  signal h : bit;
  signal jvzgc : real_vector(2 to 1);
  signal hrndcn : boolean;
  signal ytl : severity_level;
  signal gcmvat : real;
  signal gb : integer;
  signal xzsj : boolean_vector(0 to 2);
begin
  ffeicxk : entity work.bxymszvha
    port map (juqbmcxeug => xzsj, pumhi => gb, kzsglyzow => gcmvat, req => ytl);
  t : entity work.d
    port map (qtdwlq => hrndcn, dtclmv => jvzgc, guqfuabw => h, qirobu => ykpy);
  
  -- Single-driven assignments
  k <= 16#C_1.96AE3# us;
end ddinsn;



-- Seed after: 1107340798181151634,15306146742274979323
