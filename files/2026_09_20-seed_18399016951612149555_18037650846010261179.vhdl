-- Seed: 18399016951612149555,18037650846010261179

entity dlec is
  port (korqatv : inout real; e : inout boolean; ox : in real; ogtxf : buffer boolean_vector(1 to 2));
end dlec;

architecture lexmfkd of dlec is
  
begin
  -- Single-driven assignments
  ogtxf <= ogtxf;
  korqatv <= ox;
  e <= TRUE;
end lexmfkd;

entity urgmpfvmbl is
  port (uzk : inout time_vector(0 downto 0));
end urgmpfvmbl;

architecture rurldyaq of urgmpfvmbl is
  signal oefv : boolean_vector(1 to 2);
  signal mkfwtjjlh : boolean;
  signal advnxuxfuv : boolean_vector(1 to 2);
  signal rsxeyobe : real;
  signal uo : boolean;
  signal iuymuj : boolean_vector(1 to 2);
  signal tpcpvd : real;
  signal yxbe : boolean;
  signal y : real;
begin
  mqvkrgks : entity work.dlec
    port map (korqatv => y, e => yxbe, ox => tpcpvd, ogtxf => iuymuj);
  wfmvnrw : entity work.dlec
    port map (korqatv => tpcpvd, e => uo, ox => rsxeyobe, ogtxf => advnxuxfuv);
  dtq : entity work.dlec
    port map (korqatv => rsxeyobe, e => mkfwtjjlh, ox => y, ogtxf => oefv);
  
  -- Single-driven assignments
  uzk <= uzk;
end rurldyaq;

entity gavtpb is
  port (yzy : buffer bit_vector(1 downto 4));
end gavtpb;

architecture diugu of gavtpb is
  signal bmi : time_vector(0 downto 0);
begin
  qqom : entity work.urgmpfvmbl
    port map (uzk => bmi);
  
  -- Single-driven assignments
  yzy <= yzy;
end diugu;

library ieee;
use ieee.std_logic_1164.all;

entity ezoouskfr is
  port (t : buffer real; lflvguqpuh : out std_logic_vector(3 downto 1));
end ezoouskfr;

architecture mfd of ezoouskfr is
  signal kgthewz : bit_vector(1 downto 4);
  signal nmysmwcesz : boolean_vector(1 to 2);
  signal e : boolean;
  signal tfk : real;
  signal j : boolean_vector(1 to 2);
  signal vmdskifxoi : real;
  signal wgyjtruzt : boolean;
  signal pfr : real;
  signal irslo : time_vector(0 downto 0);
begin
  zngfpu : entity work.urgmpfvmbl
    port map (uzk => irslo);
  qqw : entity work.dlec
    port map (korqatv => pfr, e => wgyjtruzt, ox => vmdskifxoi, ogtxf => j);
  dki : entity work.dlec
    port map (korqatv => tfk, e => e, ox => vmdskifxoi, ogtxf => nmysmwcesz);
  fudpil : entity work.gavtpb
    port map (yzy => kgthewz);
  
  -- Single-driven assignments
  vmdskifxoi <= t;
  t <= 16#9_A_1_B_6.B_4_D#;
  
  -- Multi-driven assignments
  lflvguqpuh <= "ZUW";
  lflvguqpuh <= ('-', 'L', 'U');
  lflvguqpuh <= lflvguqpuh;
  lflvguqpuh <= ('L', '-', 'U');
end mfd;



-- Seed after: 10436283422190084316,18037650846010261179
