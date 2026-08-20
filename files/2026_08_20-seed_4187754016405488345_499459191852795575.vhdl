-- Seed: 4187754016405488345,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity dc is
  port ( xuskevbro : in std_logic_vector(0 downto 3)
  ; v : linkage std_logic
  ; oqwlfyt : inout time_vector(2 downto 0)
  ; xll : buffer std_logic_vector(3 to 2)
  );
end dc;

architecture eiowjxmrnt of dc is
  
begin
  -- Single-driven assignments
  oqwlfyt <= oqwlfyt;
  
  -- Multi-driven assignments
  xll <= "";
  xll <= xll;
end eiowjxmrnt;

entity rkcekd is
  port (iskttd : buffer real; fmbixcyhyn : buffer integer);
end rkcekd;

library ieee;
use ieee.std_logic_1164.all;

architecture tuxi of rkcekd is
  signal qwcwhly : std_logic_vector(3 to 2);
  signal bvxkvigq : time_vector(2 downto 0);
  signal ilworvyq : time_vector(2 downto 0);
  signal fyhkuqy : std_logic;
  signal lv : std_logic_vector(0 downto 3);
  signal vdfw : time_vector(2 downto 0);
  signal tkyut : std_logic;
  signal jesjdfhw : std_logic_vector(3 to 2);
  signal err : time_vector(2 downto 0);
  signal p : std_logic;
  signal xslzulg : std_logic_vector(0 downto 3);
begin
  b : entity work.dc
    port map (xuskevbro => xslzulg, v => p, oqwlfyt => err, xll => jesjdfhw);
  dbbym : entity work.dc
    port map (xuskevbro => xslzulg, v => tkyut, oqwlfyt => vdfw, xll => xslzulg);
  rwmlckzuvt : entity work.dc
    port map (xuskevbro => lv, v => fyhkuqy, oqwlfyt => ilworvyq, xll => xslzulg);
  oufkhsnz : entity work.dc
    port map (xuskevbro => xslzulg, v => p, oqwlfyt => bvxkvigq, xll => qwcwhly);
  
  -- Single-driven assignments
  fmbixcyhyn <= 3_0;
  iskttd <= iskttd;
  
  -- Multi-driven assignments
  qwcwhly <= lv;
  xslzulg <= qwcwhly;
  qwcwhly <= (others => '0');
  tkyut <= '0';
end tuxi;

library ieee;
use ieee.std_logic_1164.all;

entity mptjnp is
  port (xdlx : in std_logic_vector(2 downto 3); iwmwk : linkage std_logic; rdfmwcpum : linkage time; qojxifli : inout string(5 to 1));
end mptjnp;

library ieee;
use ieee.std_logic_1164.all;

architecture bat of mptjnp is
  signal uo : integer;
  signal nipw : real;
  signal poo : std_logic_vector(3 to 2);
  signal hutc : time_vector(2 downto 0);
  signal njpvgunno : std_logic;
  signal cw : std_logic_vector(0 downto 3);
begin
  xoyekfyao : entity work.dc
    port map (xuskevbro => cw, v => njpvgunno, oqwlfyt => hutc, xll => poo);
  kpigleo : entity work.rkcekd
    port map (iskttd => nipw, fmbixcyhyn => uo);
end bat;



-- Seed after: 16962684567596979991,499459191852795575
