-- Seed: 12712389005883752470,8118127366649987907

entity joql is
  port (kcxy : linkage real);
end joql;

architecture lzpntqsf of joql is
  
begin
  
end lzpntqsf;

entity lmxswc is
  port (hzi : in time);
end lmxswc;

architecture icjgya of lmxswc is
  signal ujfrmvjo : real;
  signal zvcltw : real;
begin
  u : entity work.joql
    port map (kcxy => zvcltw);
  uk : entity work.joql
    port map (kcxy => ujfrmvjo);
end icjgya;

entity kclcbbe is
  port (r : in severity_level; opsdqnmfs : inout time; z : linkage time);
end kclcbbe;

architecture fliabzg of kclcbbe is
  signal hrpr : real;
  signal b : real;
  signal wvthy : time;
begin
  jphuof : entity work.lmxswc
    port map (hzi => wvthy);
  jsytusvzge : entity work.joql
    port map (kcxy => b);
  jhooefuq : entity work.joql
    port map (kcxy => hrpr);
  
  -- Single-driven assignments
  wvthy <= 16#3_0_A_C.BA24# ms;
  opsdqnmfs <= 21.43 ps;
end fliabzg;

entity ozwb is
  port (aeq : linkage integer; hyauvsluh : linkage integer; llyufev : linkage character);
end ozwb;

architecture fyu of ozwb is
  signal atskcyqf : time;
  signal uyj : time;
  signal typncmzcwb : time;
  signal sbj : severity_level;
begin
  wyldc : entity work.kclcbbe
    port map (r => sbj, opsdqnmfs => typncmzcwb, z => uyj);
  dhsqvbgcp : entity work.lmxswc
    port map (hzi => atskcyqf);
end fyu;



-- Seed after: 12286453390476893955,8118127366649987907
