-- Seed: 6612938489775003597,3316342841050048249

entity d is
  port (nmczglsa : buffer severity_level; qksmbsgyu : linkage bit);
end d;

architecture ui of d is
  
begin
  -- Single-driven assignments
  nmczglsa <= FAILURE;
end ui;

entity tkjjdhi is
  port (cs : inout integer; kfircwjuzl : inout time);
end tkjjdhi;

architecture fwisl of tkjjdhi is
  signal u : bit;
  signal uehy : severity_level;
  signal nc : bit;
  signal zk : severity_level;
  signal beztcv : bit;
  signal zrrmbzk : severity_level;
  signal swp : bit;
  signal x : severity_level;
begin
  hcazfzc : entity work.d
    port map (nmczglsa => x, qksmbsgyu => swp);
  qhqr : entity work.d
    port map (nmczglsa => zrrmbzk, qksmbsgyu => beztcv);
  rtbd : entity work.d
    port map (nmczglsa => zk, qksmbsgyu => nc);
  g : entity work.d
    port map (nmczglsa => uehy, qksmbsgyu => u);
  
  -- Single-driven assignments
  cs <= 3_0;
end fwisl;



-- Seed after: 8388027493337688420,3316342841050048249
