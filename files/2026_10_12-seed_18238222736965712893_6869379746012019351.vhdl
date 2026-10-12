-- Seed: 18238222736965712893,6869379746012019351

entity wa is
  port (qzeuwdrz : inout time; lxzaogiw : buffer bit; psty : out bit; ycfcpz : buffer integer);
end wa;

architecture g of wa is
  
begin
  -- Single-driven assignments
  qzeuwdrz <= qzeuwdrz;
  psty <= '1';
  ycfcpz <= ycfcpz;
end g;

entity askb is
  port (ccitbgxxs : linkage severity_level; sjdzyjru : in integer; tvhj : buffer severity_level);
end askb;

architecture gvrbfolnz of askb is
  signal qicytgj : integer;
  signal oxcwrvf : bit;
  signal txilpcbcpy : bit;
  signal umrvczyd : time;
  signal scnay : integer;
  signal xjyen : bit;
  signal utmdxaqpwh : bit;
  signal hvuzave : time;
  signal qyknm : integer;
  signal iw : bit;
  signal bawbokqna : bit;
  signal cuxy : time;
begin
  xzaoaccmjj : entity work.wa
    port map (qzeuwdrz => cuxy, lxzaogiw => bawbokqna, psty => iw, ycfcpz => qyknm);
  lgf : entity work.wa
    port map (qzeuwdrz => hvuzave, lxzaogiw => utmdxaqpwh, psty => xjyen, ycfcpz => scnay);
  rhty : entity work.wa
    port map (qzeuwdrz => umrvczyd, lxzaogiw => txilpcbcpy, psty => oxcwrvf, ycfcpz => qicytgj);
  
  -- Single-driven assignments
  tvhj <= tvhj;
end gvrbfolnz;

entity sg is
  port (ffc : linkage real);
end sg;

architecture nlmyxjih of sg is
  signal yxupvnz : severity_level;
  signal aszqdlws : severity_level;
  signal davah : severity_level;
  signal l : integer;
  signal g : severity_level;
  signal t : integer;
  signal xdjn : bit;
  signal jmv : bit;
  signal myocshal : time;
  signal ldruyisg : integer;
  signal daaiw : bit;
  signal anwreaalfm : bit;
  signal ouqyqkauvt : time;
begin
  epgjvzq : entity work.wa
    port map (qzeuwdrz => ouqyqkauvt, lxzaogiw => anwreaalfm, psty => daaiw, ycfcpz => ldruyisg);
  awmcwm : entity work.wa
    port map (qzeuwdrz => myocshal, lxzaogiw => jmv, psty => xdjn, ycfcpz => t);
  chjn : entity work.askb
    port map (ccitbgxxs => g, sjdzyjru => l, tvhj => davah);
  mvkc : entity work.askb
    port map (ccitbgxxs => aszqdlws, sjdzyjru => ldruyisg, tvhj => yxupvnz);
  
  -- Single-driven assignments
  l <= ldruyisg;
end nlmyxjih;



-- Seed after: 14776901877213942619,6869379746012019351
