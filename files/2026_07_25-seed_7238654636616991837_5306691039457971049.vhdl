-- Seed: 7238654636616991837,5306691039457971049

entity ttnjwdyxr is
  port (pwpft : in severity_level);
end ttnjwdyxr;

architecture dx of ttnjwdyxr is
  
begin
  
end dx;

entity dtglzvb is
  port (fapy : out integer; cs : inout time; kjy : buffer time);
end dtglzvb;

architecture grcbj of dtglzvb is
  signal grctyjuwa : severity_level;
  signal teyevemxs : severity_level;
  signal glawgvtdd : severity_level;
begin
  yjhaqfxcb : entity work.ttnjwdyxr
    port map (pwpft => glawgvtdd);
  jdhocyns : entity work.ttnjwdyxr
    port map (pwpft => teyevemxs);
  hxxumrp : entity work.ttnjwdyxr
    port map (pwpft => teyevemxs);
  fbp : entity work.ttnjwdyxr
    port map (pwpft => grctyjuwa);
  
  -- Single-driven assignments
  kjy <= kjy;
  glawgvtdd <= glawgvtdd;
  fapy <= 2#0000#;
  teyevemxs <= FAILURE;
  cs <= 0 hr;
end grcbj;

library ieee;
use ieee.std_logic_1164.all;

entity ul is
  port (cjqr : out real_vector(1 downto 0); b : linkage std_logic_vector(3 downto 2); qvhpniqvcn : buffer real; wmozlr : linkage real);
end ul;

architecture mryoao of ul is
  signal buekgfxfx : time;
  signal h : time;
  signal xjddl : integer;
  signal xeue : time;
  signal nsf : time;
  signal y : integer;
  signal vszkw : time;
  signal jpjvtr : time;
  signal fofqkcqcdn : integer;
begin
  vugn : entity work.dtglzvb
    port map (fapy => fofqkcqcdn, cs => jpjvtr, kjy => vszkw);
  afppeooveg : entity work.dtglzvb
    port map (fapy => y, cs => nsf, kjy => xeue);
  ofb : entity work.dtglzvb
    port map (fapy => xjddl, cs => h, kjy => buekgfxfx);
  
  -- Single-driven assignments
  qvhpniqvcn <= qvhpniqvcn;
  cjqr <= (16#81.EB#, 8#2.1#);
end mryoao;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (kkfzd : out real; l : linkage std_logic_vector(4 to 2));
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture wwjxxingxe of g is
  signal m : severity_level;
  signal iqc : severity_level;
  signal pcacokrijq : real;
  signal mo : std_logic_vector(3 downto 2);
  signal lxglwyz : real_vector(1 downto 0);
begin
  pksjtzp : entity work.ul
    port map (cjqr => lxglwyz, b => mo, qvhpniqvcn => kkfzd, wmozlr => pcacokrijq);
  bmbipo : entity work.ttnjwdyxr
    port map (pwpft => iqc);
  skdgjbp : entity work.ttnjwdyxr
    port map (pwpft => m);
  aeqhnjpr : entity work.ttnjwdyxr
    port map (pwpft => iqc);
  
  -- Single-driven assignments
  m <= m;
  iqc <= iqc;
  
  -- Multi-driven assignments
  mo <= "LX";
  mo <= ('-', 'U');
  mo <= mo;
  mo <= "X0";
end wwjxxingxe;



-- Seed after: 16292856154513774094,5306691039457971049
