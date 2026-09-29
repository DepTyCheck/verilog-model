-- Seed: 16838610919978948907,10940991575366938685

entity zt is
  port (d : buffer integer);
end zt;

architecture qzifjit of zt is
  
begin
  
end qzifjit;

entity nvp is
  port (fsfkheo : in time; yqotb : out integer; wf : in bit; jle : inout boolean);
end nvp;

architecture skrci of nvp is
  signal iehmb : integer;
  signal jeqscb : integer;
begin
  uxdsmcdjc : entity work.zt
    port map (d => yqotb);
  ncojbqw : entity work.zt
    port map (d => jeqscb);
  klbg : entity work.zt
    port map (d => iehmb);
  
  -- Single-driven assignments
  jle <= jle;
end skrci;

entity ucse is
  port (d : inout time_vector(1 downto 2); hiafqizhd : out severity_level; hhb : inout severity_level; mmbztulrss : in time);
end ucse;

architecture dghvv of ucse is
  signal ayroqrrygx : integer;
  signal kbmaz : integer;
begin
  ccpnucw : entity work.zt
    port map (d => kbmaz);
  qyeyple : entity work.zt
    port map (d => ayroqrrygx);
  
  -- Single-driven assignments
  d <= d;
  hhb <= ERROR;
  hiafqizhd <= hhb;
end dghvv;

entity zvanzd is
  port (klkc : out time; ius : out real);
end zvanzd;

architecture gv of zvanzd is
  signal jbmjfyszid : time;
  signal ojuhfwr : severity_level;
  signal zlm : severity_level;
  signal bcjv : time_vector(1 downto 2);
  signal ai : integer;
  signal knykq : boolean;
  signal qv : bit;
  signal myqdbkoby : integer;
  signal kiycgorf : time;
  signal rz : severity_level;
  signal az : severity_level;
  signal wprsahqj : time_vector(1 downto 2);
begin
  ytybbpb : entity work.ucse
    port map (d => wprsahqj, hiafqizhd => az, hhb => rz, mmbztulrss => kiycgorf);
  ly : entity work.nvp
    port map (fsfkheo => kiycgorf, yqotb => myqdbkoby, wf => qv, jle => knykq);
  louaewe : entity work.zt
    port map (d => ai);
  np : entity work.ucse
    port map (d => bcjv, hiafqizhd => zlm, hhb => ojuhfwr, mmbztulrss => jbmjfyszid);
  
  -- Single-driven assignments
  klkc <= 2 sec;
  jbmjfyszid <= 0 min;
  qv <= qv;
  kiycgorf <= 16#F_7# us;
  ius <= 8#3311.214#;
end gv;



-- Seed after: 7774076764724112593,10940991575366938685
