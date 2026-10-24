-- Seed: 10439117982708101165,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity jxqaeio is
  port (tqdw : linkage std_logic; dnlqvs : in std_logic);
end jxqaeio;

architecture firxaokik of jxqaeio is
  
begin
  
end firxaokik;

library ieee;
use ieee.std_logic_1164.all;

entity wpky is
  port (mgtoni : linkage integer; xed : in integer; q : buffer std_logic; rvudaabsdb : in time);
end wpky;

library ieee;
use ieee.std_logic_1164.all;

architecture xqtgb of wpky is
  signal pgt : std_logic;
begin
  ntnghoxpu : entity work.jxqaeio
    port map (tqdw => pgt, dnlqvs => q);
  
  -- Multi-driven assignments
  q <= 'Z';
  q <= q;
  q <= pgt;
  q <= q;
end xqtgb;

entity fcbtpvsbl is
  port (bvanpde : inout bit; vlfg : inout real; rwzimbvk : linkage real; uteiacoaht : in real);
end fcbtpvsbl;

library ieee;
use ieee.std_logic_1164.all;

architecture lz of fcbtpvsbl is
  signal levwoefvd : time;
  signal nzohmjci : std_logic;
  signal klzmdxva : integer;
begin
  wkppaq : entity work.wpky
    port map (mgtoni => klzmdxva, xed => klzmdxva, q => nzohmjci, rvudaabsdb => levwoefvd);
  awa : entity work.jxqaeio
    port map (tqdw => nzohmjci, dnlqvs => nzohmjci);
  yyxmfgkf : entity work.jxqaeio
    port map (tqdw => nzohmjci, dnlqvs => nzohmjci);
  cfkc : entity work.jxqaeio
    port map (tqdw => nzohmjci, dnlqvs => nzohmjci);
  
  -- Single-driven assignments
  vlfg <= 8#0_5_0_3_7.0_4_7_5_1#;
  bvanpde <= bvanpde;
  
  -- Multi-driven assignments
  nzohmjci <= nzohmjci;
  nzohmjci <= nzohmjci;
end lz;



-- Seed after: 3796302598160067580,10975587770517081373
