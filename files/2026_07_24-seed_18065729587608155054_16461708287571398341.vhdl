-- Seed: 18065729587608155054,16461708287571398341

entity wwikibp is
  port (vmcs : out character; jcd : buffer time);
end wwikibp;

architecture qq of wwikibp is
  
begin
  -- Single-driven assignments
  jcd <= 1 hr;
  vmcs <= vmcs;
end qq;



-- Seed after: 9439022077747068360,16461708287571398341
