-- Seed: 16743960053444036200,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity jn is
  port (nblmh : buffer bit_vector(3 downto 0); f : in std_logic_vector(4 downto 3); uzxkrzk : inout boolean);
end jn;

architecture g of jn is
  
begin
  -- Single-driven assignments
  uzxkrzk <= FALSE;
  nblmh <= ('1', '0', '1', '0');
end g;

library ieee;
use ieee.std_logic_1164.all;

entity evctzchls is
  port (fomaabwdcx : in std_logic_vector(0 downto 1); ncbpkdxv : in integer; zgonnttfr : inout bit_vector(2 to 2));
end evctzchls;

library ieee;
use ieee.std_logic_1164.all;

architecture exwqpl of evctzchls is
  signal eonp : boolean;
  signal ecoxwiokn : bit_vector(3 downto 0);
  signal bqjiy : boolean;
  signal hqv : bit_vector(3 downto 0);
  signal rzfanyy : boolean;
  signal ufcfhhh : std_logic_vector(4 downto 3);
  signal dlmfshpgg : bit_vector(3 downto 0);
begin
  kpzh : entity work.jn
    port map (nblmh => dlmfshpgg, f => ufcfhhh, uzxkrzk => rzfanyy);
  fqisbrhgxs : entity work.jn
    port map (nblmh => hqv, f => ufcfhhh, uzxkrzk => bqjiy);
  o : entity work.jn
    port map (nblmh => ecoxwiokn, f => ufcfhhh, uzxkrzk => eonp);
  
  -- Multi-driven assignments
  ufcfhhh <= ufcfhhh;
end exwqpl;

entity lhvvsn is
  port (mtf : buffer real; xvnymor : linkage time; vmgo : in time);
end lhvvsn;

architecture utt of lhvvsn is
  
begin
  
end utt;

entity ntl is
  port (uxciupp : buffer time_vector(0 to 4); zlo : in time);
end ntl;

library ieee;
use ieee.std_logic_1164.all;

architecture n of ntl is
  signal xmvqd : boolean;
  signal depwu : std_logic_vector(4 downto 3);
  signal hdnyoz : bit_vector(3 downto 0);
  signal clto : boolean;
  signal u : std_logic_vector(4 downto 3);
  signal zuduoasj : bit_vector(3 downto 0);
  signal rhn : time;
  signal otagheefxr : real;
begin
  ki : entity work.lhvvsn
    port map (mtf => otagheefxr, xvnymor => rhn, vmgo => zlo);
  ofsa : entity work.jn
    port map (nblmh => zuduoasj, f => u, uzxkrzk => clto);
  adk : entity work.jn
    port map (nblmh => hdnyoz, f => depwu, uzxkrzk => xmvqd);
  
  -- Single-driven assignments
  uxciupp <= (16#F_5_9.2_C_4_0_D# fs, 16#A_8# us, 3_2_1_1.0_2_4_0_3 ps, 32233.342 ps, 3 min);
  
  -- Multi-driven assignments
  u <= "UZ";
  u <= u;
end n;



-- Seed after: 9344299609321632676,5983430343285687595
