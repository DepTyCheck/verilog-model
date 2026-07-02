-- Seed: 17930631880828721587,13694093582652240945

entity asqum is
  port (uyicbrltt : inout real; lxtrivuay : out time; vkkzbnpd : linkage time; nv : linkage string(1 to 1));
end asqum;

architecture cxkkiaojfx of asqum is
  
begin
  -- Single-driven assignments
  lxtrivuay <= 0.04 fs;
  uyicbrltt <= 2#01100.1_1_1_1_0#;
end cxkkiaojfx;

entity rvlsjcgo is
  port (wqnn : inout time; uchsxa : buffer time_vector(2 to 3); xoxcng : out time);
end rvlsjcgo;

architecture mejqen of rvlsjcgo is
  signal dx : string(1 to 1);
  signal ik : time;
  signal vdplejlgm : real;
  signal qtsh : string(1 to 1);
  signal gdtulbos : time;
  signal kihibe : real;
  signal r : string(1 to 1);
  signal q : time;
  signal nqq : time;
  signal drbr : real;
  signal xwq : string(1 to 1);
  signal qz : time;
  signal zq : time;
  signal rlqcfwewl : real;
begin
  emfetvciko : entity work.asqum
    port map (uyicbrltt => rlqcfwewl, lxtrivuay => zq, vkkzbnpd => qz, nv => xwq);
  irhoeubv : entity work.asqum
    port map (uyicbrltt => drbr, lxtrivuay => nqq, vkkzbnpd => q, nv => r);
  waoajartt : entity work.asqum
    port map (uyicbrltt => kihibe, lxtrivuay => xoxcng, vkkzbnpd => gdtulbos, nv => qtsh);
  hti : entity work.asqum
    port map (uyicbrltt => vdplejlgm, lxtrivuay => wqnn, vkkzbnpd => ik, nv => dx);
end mejqen;



-- Seed after: 14089233015874856420,13694093582652240945
