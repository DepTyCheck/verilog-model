-- Seed: 16109607642546687128,7808623373429384027

entity ygjxbesev is
  port (rinj : inout integer; ee : in integer_vector(0 to 1); vsxhu : buffer integer; aypmhxqnl : inout time);
end ygjxbesev;

architecture gvyaflxsro of ygjxbesev is
  
begin
  -- Single-driven assignments
  aypmhxqnl <= 16#D_D_0_7# ns;
  rinj <= vsxhu;
  vsxhu <= 01;
end gvyaflxsro;

entity q is
  port (pde : out time);
end q;

architecture qcbwhi of q is
  signal pmstyvfkd : time;
  signal isqzlid : integer;
  signal zpubuqa : integer_vector(0 to 1);
  signal na : integer;
  signal ql : time;
  signal uvwynb : integer;
  signal ry : integer_vector(0 to 1);
  signal gldop : integer;
  signal qklxwnsylb : time;
  signal vmbcmwsjp : integer;
  signal b : integer;
  signal dccaatv : integer;
  signal jcktqhjvon : integer_vector(0 to 1);
  signal zjwtsikbqh : integer;
begin
  nz : entity work.ygjxbesev
    port map (rinj => zjwtsikbqh, ee => jcktqhjvon, vsxhu => dccaatv, aypmhxqnl => pde);
  cftwy : entity work.ygjxbesev
    port map (rinj => b, ee => jcktqhjvon, vsxhu => vmbcmwsjp, aypmhxqnl => qklxwnsylb);
  f : entity work.ygjxbesev
    port map (rinj => gldop, ee => ry, vsxhu => uvwynb, aypmhxqnl => ql);
  ykahstyqu : entity work.ygjxbesev
    port map (rinj => na, ee => zpubuqa, vsxhu => isqzlid, aypmhxqnl => pmstyvfkd);
  
  -- Single-driven assignments
  jcktqhjvon <= (2#001#, 8#4_3#);
  zpubuqa <= (4, 16#0_C_F#);
  ry <= (16#3AA28#, 0_1_1);
end qcbwhi;



-- Seed after: 17788689565061059278,7808623373429384027
