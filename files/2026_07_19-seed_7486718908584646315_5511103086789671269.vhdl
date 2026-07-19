-- Seed: 7486718908584646315,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity nws is
  port ( ybweibvm : inout std_logic_vector(4 downto 4)
  ; osvypm : inout real_vector(1 to 2)
  ; yesyvwjlf : out boolean_vector(1 downto 2)
  ; ovh : linkage real
  );
end nws;

architecture suqf of nws is
  
begin
  -- Single-driven assignments
  yesyvwjlf <= (others => TRUE);
  osvypm <= osvypm;
  
  -- Multi-driven assignments
  ybweibvm <= ybweibvm;
end suqf;

entity rptulofuk is
  port (pygmewgi : buffer time; mkc : in real);
end rptulofuk;

library ieee;
use ieee.std_logic_1164.all;

architecture u of rptulofuk is
  signal eyug : real;
  signal gueoi : boolean_vector(1 downto 2);
  signal dpx : real_vector(1 to 2);
  signal bn : real;
  signal sdtxz : boolean_vector(1 downto 2);
  signal oticsvek : real_vector(1 to 2);
  signal raoknzkd : real;
  signal sugtk : boolean_vector(1 downto 2);
  signal vaisnstlb : real_vector(1 to 2);
  signal kkhccu : std_logic_vector(4 downto 4);
begin
  xbdq : entity work.nws
    port map (ybweibvm => kkhccu, osvypm => vaisnstlb, yesyvwjlf => sugtk, ovh => raoknzkd);
  ajjzdv : entity work.nws
    port map (ybweibvm => kkhccu, osvypm => oticsvek, yesyvwjlf => sdtxz, ovh => bn);
  pkw : entity work.nws
    port map (ybweibvm => kkhccu, osvypm => dpx, yesyvwjlf => gueoi, ovh => eyug);
  
  -- Multi-driven assignments
  kkhccu <= (others => '-');
  kkhccu <= kkhccu;
end u;

entity sz is
  port (eknoar : in integer; e : buffer real; rnvkvaaadn : in bit_vector(3 to 1));
end sz;

library ieee;
use ieee.std_logic_1164.all;

architecture xvx of sz is
  signal fyjtbhk : boolean_vector(1 downto 2);
  signal zocj : real_vector(1 to 2);
  signal opkmbf : std_logic_vector(4 downto 4);
begin
  goibpfzzf : entity work.nws
    port map (ybweibvm => opkmbf, osvypm => zocj, yesyvwjlf => fyjtbhk, ovh => e);
  
  -- Multi-driven assignments
  opkmbf <= opkmbf;
  opkmbf <= opkmbf;
  opkmbf <= opkmbf;
end xvx;

library ieee;
use ieee.std_logic_1164.all;

entity imszts is
  port (bqbnfs : linkage std_logic_vector(4 downto 1));
end imszts;

architecture thvykpapq of imszts is
  signal tmtpthfr : real;
  signal lpzmriyymw : time;
begin
  yfih : entity work.rptulofuk
    port map (pygmewgi => lpzmriyymw, mkc => tmtpthfr);
  
  -- Single-driven assignments
  tmtpthfr <= 4.3;
end thvykpapq;



-- Seed after: 8204227731924492827,5511103086789671269
