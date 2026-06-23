-- Seed: 11201656460364710474,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity ihxup is
  port (daecuwbh : buffer boolean; mwrsrxron : in integer; iubnqib : in std_logic; b : out real);
end ihxup;

architecture bjxvjbo of ihxup is
  
begin
  -- Single-driven assignments
  daecuwbh <= TRUE;
end bjxvjbo;

library ieee;
use ieee.std_logic_1164.all;

entity bnnwtl is
  port (m : out std_logic; iqktwok : buffer real; ovyvg : inout real; c : linkage time);
end bnnwtl;

library ieee;
use ieee.std_logic_1164.all;

architecture nz of bnnwtl is
  signal rmayici : real;
  signal rmm : std_logic;
  signal xrigaevs : integer;
  signal ulavdbt : boolean;
  signal pc : integer;
  signal rygjuao : boolean;
  signal ztsde : real;
  signal vrqzrged : integer;
  signal k : boolean;
begin
  fvpdoqiqze : entity work.ihxup
    port map (daecuwbh => k, mwrsrxron => vrqzrged, iubnqib => m, b => ztsde);
  ninvy : entity work.ihxup
    port map (daecuwbh => rygjuao, mwrsrxron => pc, iubnqib => m, b => ovyvg);
  qtt : entity work.ihxup
    port map (daecuwbh => ulavdbt, mwrsrxron => xrigaevs, iubnqib => rmm, b => rmayici);
  
  -- Multi-driven assignments
  m <= 'W';
end nz;

library ieee;
use ieee.std_logic_1164.all;

entity wgyfgwgix is
  port (qxd : buffer time; pbctlwfo : linkage std_logic_vector(1 to 4); ywgei : linkage integer; jcijdo : inout std_logic_vector(1 to 0));
end wgyfgwgix;

architecture xu of wgyfgwgix is
  
begin
  -- Single-driven assignments
  qxd <= 2233 ms;
  
  -- Multi-driven assignments
  jcijdo <= (others => '0');
end xu;

library ieee;
use ieee.std_logic_1164.all;

entity sf is
  port (geuesjrt : in std_logic; v : inout std_logic_vector(0 downto 4); igtcuvo : in real);
end sf;

library ieee;
use ieee.std_logic_1164.all;

architecture xrngywbngr of sf is
  signal jynzewfhc : time;
  signal d : real;
  signal jqcqcs : real;
  signal plnzvt : time;
  signal pq : real;
  signal dbeghbbgc : real;
  signal eixukm : std_logic;
  signal uz : real;
  signal ydwfouruvn : std_logic;
  signal hncsiwlhc : integer;
  signal pzyqk : boolean;
begin
  nnpkddtb : entity work.ihxup
    port map (daecuwbh => pzyqk, mwrsrxron => hncsiwlhc, iubnqib => ydwfouruvn, b => uz);
  bto : entity work.bnnwtl
    port map (m => eixukm, iqktwok => dbeghbbgc, ovyvg => pq, c => plnzvt);
  t : entity work.bnnwtl
    port map (m => eixukm, iqktwok => jqcqcs, ovyvg => d, c => jynzewfhc);
end xrngywbngr;



-- Seed after: 17444039317613749368,8421704836678237495
