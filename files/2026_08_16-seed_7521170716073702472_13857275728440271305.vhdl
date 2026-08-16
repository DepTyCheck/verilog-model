-- Seed: 7521170716073702472,13857275728440271305

entity owxuehizcg is
  port (gqmndcphn : out real; iyekr : buffer time; m : inout real);
end owxuehizcg;

architecture pq of owxuehizcg is
  
begin
  -- Single-driven assignments
  m <= 16#0_8_5_5.5AC#;
  gqmndcphn <= m;
  iyekr <= iyekr;
end pq;

entity t is
  port (kwkrpv : inout real; hzswxboggb : out character);
end t;

architecture ej of t is
  signal shf : real;
  signal q : time;
  signal xgbbvuingq : real;
  signal njbvmat : time;
  signal rclcjcnpb : real;
  signal fusw : real;
  signal xbnqopcy : time;
  signal ajem : real;
begin
  mmarsul : entity work.owxuehizcg
    port map (gqmndcphn => ajem, iyekr => xbnqopcy, m => fusw);
  vstqe : entity work.owxuehizcg
    port map (gqmndcphn => rclcjcnpb, iyekr => njbvmat, m => xgbbvuingq);
  hfag : entity work.owxuehizcg
    port map (gqmndcphn => kwkrpv, iyekr => q, m => shf);
  
  -- Single-driven assignments
  hzswxboggb <= 'u';
end ej;



-- Seed after: 4292879928120813853,13857275728440271305
